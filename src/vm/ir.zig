const std = @import("std");
const Val = @import("val.zig").Val;
const Ast = @import("ast.zig").Ast;
const AstCollection = @import("ast.zig").AstCollection;
const Leaf = @import("ast.zig").Leaf;
const Heap = @import("heap.zig").Heap;

const IrBuilder = struct {
    arg_to_idx: std.StringHashMap(usize),

    const Error = Ir.Error;

    /// Initialize an Ir from an AST.
    pub fn init(self: *IrBuilder, ast: *const Ast, heap: *Heap) !*Ir {
        switch (ast.*) {
            .leaf => return self.initConstant(&ast.leaf, heap),
            .tree => |asts| {
                if (asts.len == 0) {
                    return Error.EmptyFunctionCall;
                }
                const first = &asts[0];
                const rest = asts[1..];
                switch (first.*) {
                    .leaf => |l| {
                        switch (l) {
                            .keyword => |k| switch (k) {
                                .if_expr => {
                                    switch (rest.len) {
                                        0 | 1 => return Error.NotEnoughArguments,
                                        2 => return self.initIfExpression(&rest[0], &rest[1], null, heap),
                                        3 => return self.initIfExpression(&rest[0], &rest[1], &rest[2], heap),
                                        else => return Error.TooManyArguments,
                                    }
                                },
                                .lambda => {
                                    if (rest.len < 2) {
                                        return Error.NotEnoughArguments;
                                    }
                                    return self.initLambdaExpr(&rest[0], rest[1..], heap);
                                },
                            },
                            else => return self.initFunctionCall(first, rest, heap),
                        }
                    },
                    else => return self.initFunctionCall(first, rest, heap),
                }
            },
        }
    }

    pub fn deinit(self: *IrBuilder) void {
        self.arg_to_idx.deinit();
    }

    /// Initialize an Ir from a single AST leaf.
    fn initConstant(_: *IrBuilder, leaf: *const Leaf, heap: *Heap) Error!*Ir {
        const v = switch (leaf.*) {
            Leaf.keyword => |k| switch (k) {
                .if_expr => return Error.UnexpectedIf,
                .lambda => return Error.UnexpectedLambda,
            },
            Leaf.identifier => |ident| try heap.allocGlobalSymbol(ident),
            Leaf.string => try heap.allocGlobalString(leaf.string),
            Leaf.boolean => Val{ .boolean = leaf.boolean },
            Leaf.int => Val{ .int = leaf.int },
            Leaf.float => Val{ .float = leaf.float },
        };
        const ret = try heap.allocator.create(Ir);
        ret.* = .{ .constant = v };
        return ret;
    }

    /// Initialize an Ir containing a function call.
    fn initFunctionCall(self: *IrBuilder, func_ast: *const Ast, args_ast: []const Ast, heap: *Heap) Error!*Ir {
        const function = try self.init(func_ast, heap);
        errdefer function.deinit(heap.allocator);
        var args = try std.ArrayListUnmanaged(*Ir).initCapacity(heap.allocator, args_ast.len);
        errdefer args.deinit(heap.allocator);
        errdefer for (args.items) |*a| a.*.deinit(heap.allocator);
        for (args_ast) |*a| {
            args.appendAssumeCapacity(try self.init(a, heap));
        }
        const ret = try heap.allocator.create(Ir);
        ret.* = .{ .function_call = .{
            .function = function,
            .args = try args.toOwnedSlice(heap.allocator),
        } };
        return ret;
    }

    /// Initialize an Ir containing a function call.
    fn initIfExpression(self: *IrBuilder, pred_expr: *const Ast, true_expr: *const Ast, false_expr: ?*const Ast, heap: *Heap) Error!*Ir {
        const pred_ir = try self.init(pred_expr, heap);
        errdefer pred_ir.deinit(heap.allocator);

        const true_ir = try self.init(true_expr, heap);
        errdefer true_ir.deinit(heap.allocator);

        const false_ir = if (false_expr) |e| try self.init(e, heap) else null;
        errdefer if (false_ir) |i| i.deinit(heap.allocator);

        const ret = try heap.allocator.create(Ir);
        ret.* = .{ .if_expr = .{
            .predicate = pred_ir,
            .true_expr = true_ir,
            .false_expr = false_ir,
        } };
        return ret;
    }

    /// Initialize an Ir containing a lambda definition.
    fn initLambdaExpr(_: *IrBuilder, arguments: *const Ast, body: []const Ast, heap: *Heap) Error!*Ir {
        if (body.len == 0) {
            return Error.ExpectedExpr;
        }
        var lambda_builder = IrBuilder{
            .arg_to_idx = std.StringHashMap(usize).init(heap.allocator),
        };
        defer lambda_builder.deinit();
        switch (arguments.*) {
            .leaf => return Error.ExpectedIdentifierList,
            .tree => |t| {
                for (0.., t) |arg_idx, arg_name_ast| {
                    switch (arg_name_ast) {
                        .tree => return Error.ExpectedIdentifier,
                        .leaf => |l| switch (l) {
                            .identifier => |ident| try lambda_builder.arg_to_idx.put(ident, arg_idx),
                            else => return Error.ExpectedIdentifier,
                        },
                    }
                }
            },
        }
        var exprs = try heap.allocator.alloc(*Ir, body.len);
        errdefer for (exprs) |e| e.deinit(heap.allocator);
        errdefer heap.allocator.free(exprs);
        for (0.., body) |i, *b| {
            const expr = try lambda_builder.init(b, heap);
            exprs[i] = expr;
        }

        const ret = try heap.allocator.create(Ir);
        ret.* = .{ .lambda = .{
            .exprs = exprs,
        } };
        return ret;
    }
};

/// Holds the intermediate representation. This is somewhere between the AST and bytecode in level
/// of complexity.
pub const Ir = union(enum) {
    /// A single constant value.
    constant: Val,
    /// A function call.
    function_call: struct {
        /// The function to call.
        function: *Ir,
        /// The arguments to the function.
        args: []*Ir,
    },
    /// Construct an if expression.
    if_expr: struct {
        /// The predicate that will be evaluated.
        predicate: *Ir,
        /// The block to return on true.
        true_expr: *Ir,
        /// The block to return on false or null if Val.void should be returned.
        false_expr: ?*Ir,
    },
    lambda: struct {
        exprs: []*Ir,
    },

    pub const Error = std.mem.Allocator.Error || error{
        EmptyFunctionCall,
        TooManyArguments,
        NotEnoughArguments,
        EmptyExpr,
        UnexpectedIf,
        UnexpectedLambda,
        NotImplemented,
        ExpectedExpr,
        ExpectedIdentifier,
        ExpectedIdentifierList,
    };

    /// Deallocate Ir and all related memory.
    pub fn deinit(self: *Ir, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .constant => {},
            .function_call => |*f| {
                f.function.deinit(alloc);
                for (f.args) |*a| a.*.deinit(alloc);
                alloc.free(f.args);
            },
            .if_expr => |*expr| {
                expr.predicate.deinit(alloc);
                expr.true_expr.deinit(alloc);
                if (expr.false_expr) |e| e.deinit(alloc);
            },
            .lambda => |l| {
                for (l.exprs) |e| e.deinit(alloc);
                alloc.free(l.exprs);
            },
        }
        alloc.destroy(self);
    }

    /// Initialize an Ir from a string expression.
    pub fn initStrExpr(expr: []const u8, heap: *Heap) !*Ir {
        var asts = try AstCollection.initWithStr(expr, heap.allocator);
        defer asts.deinit();
        if (asts.asts.len == 0) return Error.EmptyExpr;
        if (asts.asts.len > 1) return Error.TooManyArguments;
        return Ir.init(&asts.asts[0], heap);
    }

    /// Initialize an Ir from an AST.
    pub fn init(ast: *const Ast, heap: *Heap) !*Ir {
        var builder = IrBuilder{
            .arg_to_idx = std.StringHashMap(usize).init(heap.allocator),
        };
        return builder.init(ast, heap);
    }
};

test "parse constant expression" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try Ir.initStrExpr("1", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&Ir{ .constant = Val{ .int = 1 } }, actual);
}

test "parse simple expression" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try Ir.initStrExpr("(+ 1 2)", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&Ir{ .function_call = .{
        .function = @constCast(&Ir{ .constant = try heap.allocGlobalSymbol("+") }),
        .args = @constCast(&[_]*Ir{
            @constCast(&Ir{ .constant = .{ .int = 1 } }),
            @constCast(&Ir{ .constant = .{ .int = 2 } }),
        }),
    } }, actual);
}

test "parse if expression" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try Ir.initStrExpr("(if true 1 2)", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&Ir{ .if_expr = .{
        .predicate = @constCast(&Ir{ .constant = .{ .boolean = true } }),
        .true_expr = @constCast(&Ir{ .constant = .{ .int = 1 } }),
        .false_expr = @constCast(&Ir{ .constant = .{ .int = 2 } }),
    } }, actual);
}

test "parse if expression with no false branch" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try Ir.initStrExpr("(if true 1)", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&Ir{ .if_expr = .{
        .predicate = @constCast(&Ir{ .constant = .{ .boolean = true } }),
        .true_expr = @constCast(&Ir{ .constant = .{ .int = 1 } }),
        .false_expr = null,
    } }, actual);
}

test "parse lambda" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try Ir.initStrExpr("(lambda (a b) (+ a b) (- a b))", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&Ir{
        .lambda = .{
            .exprs = @constCast(&[_]*Ir{
                @constCast(&Ir{
                    .function_call = .{
                        .function = @constCast(&Ir{ .constant = try heap.allocGlobalSymbol("+") }),
                        .args = @constCast(&[_]*Ir{
                            @constCast(&Ir{ .constant = try heap.allocGlobalSymbol("a") }),
                            @constCast(&Ir{ .constant = try heap.allocGlobalSymbol("b") }),
                        }),
                    },
                }),
                @constCast(&Ir{
                    .function_call = .{
                        .function = @constCast(&Ir{ .constant = try heap.allocGlobalSymbol("-") }),
                        .args = @constCast(&[_]*Ir{
                            @constCast(&Ir{ .constant = try heap.allocGlobalSymbol("a") }),
                            @constCast(&Ir{ .constant = try heap.allocGlobalSymbol("b") }),
                        }),
                    },
                }),
            }),
        },
    }, actual);
}

test "lambda with no body produces error" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    try std.testing.expectError(error.NotEnoughArguments, Ir.initStrExpr("(lambda (a b))", &heap));
}

test "lambda with no arguments produces error" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    try std.testing.expectError(error.NotEnoughArguments, Ir.initStrExpr("(lambda)", &heap));
}

test "lambda with improper args produces error" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var works = try Ir.initStrExpr("(lambda () true)", &heap);
    works.deinit(std.testing.allocator);
    try std.testing.expectError(error.ExpectedIdentifierList, Ir.initStrExpr("(lambda not-a-list true)", &heap));
}

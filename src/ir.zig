const std = @import("std");
const Val = @import("val.zig").Val;
const Ast = @import("ast.zig").Ast;
const AstCollection = @import("ast.zig").AstCollection;
const AstType = @import("ast.zig").AstType;
const Leaf = @import("ast.zig").Leaf;
const LeafType = @import("ast.zig").LeafType;

/// The type for the intermediate representation.
pub const IrType = enum {
    /// A single constant value.
    constant,
    /// A function call.
    function_call,
};

/// Holds the intermediate representation. This is somewhere between the AST and bytecode in level
/// of complexity.
pub const Ir = union(IrType) {
    /// A single constant value.
    constant: Val,
    /// A function call.
    function_call: struct {
        /// The function to call.
        function: *Ir,
        /// The arguments to the function.
        args: []*Ir,
    },

    pub const Error = std.mem.Allocator.Error || error{ EmptyFunctionCall, TooManyExprs, EmptyExpr };

    /// Deallocate Ir and all related memory.
    pub fn deinit(self: *Ir, alloc: std.mem.Allocator) void {
        switch (self.*) {
            IrType.constant => self.constant.deinit(alloc),
            IrType.function_call => |*f| {
                f.function.deinit(alloc);
                for (f.args) |*a| a.*.deinit(alloc);
                alloc.free(f.args);
            },
        }
        alloc.destroy(self);
    }

    /// Initialize an Ir from a string expression.
    pub fn initStrExpr(expr: []const u8, alloc: std.mem.Allocator) !*Ir {
        var asts = try AstCollection.initWithStr(expr, alloc);
        defer asts.deinit();
        if (asts.asts.len == 0) return Error.EmptyExpr;
        if (asts.asts.len > 1) return Error.TooManyExprs;
        return Ir.init(&asts.asts[0], alloc);
    }

    /// Initialize an Ir from an AST.
    pub fn init(ast: *const Ast, alloc: std.mem.Allocator) !*Ir {
        switch (ast.*) {
            AstType.leaf => return Ir.initConstant(&ast.leaf, alloc),
            AstType.tree => |asts| return Ir.initFunctionCall(asts, alloc),
        }
    }

    /// Initialize an Ir from a single AST leaf.
    fn initConstant(leaf: *const Leaf, alloc: std.mem.Allocator) Error!*Ir {
        const v = switch (leaf.*) {
            LeafType.identifier => Val{ .symbol = leaf.identifier },
            LeafType.string => try Val.initStr(leaf.string, alloc),
            LeafType.int => Val{ .int = leaf.int },
            LeafType.float => Val{ .float = leaf.float },
        };
        const ret = try alloc.create(Ir);
        ret.* = .{ .constant = v };
        return ret;
    }

    /// Initialize an Ir containing a function call.
    fn initFunctionCall(asts: []const Ast, alloc: std.mem.Allocator) Error!*Ir {
        if (asts.len == 0) {
            return Error.EmptyFunctionCall;
        }
        const function = try init(&asts[0], alloc);
        errdefer function.deinit(alloc);
        var args = try std.ArrayListUnmanaged(*Ir).initCapacity(alloc, asts.len - 1);
        errdefer args.deinit(alloc);
        errdefer for (args.items) |*a| a.*.deinit(alloc);
        for (asts[1..]) |*a| {
            try args.append(alloc, try init(a, alloc));
        }
        const ret = try alloc.create(Ir);
        ret.* = .{ .function_call = .{
            .function = function,
            .args = try args.toOwnedSlice(alloc),
        } };
        return ret;
    }
};

test "parse constant expression" {
    var actual = try Ir.initStrExpr("1", std.testing.allocator);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&Ir{ .constant = Val{ .int = 1 } }, actual);
}

test "parse simple expression" {
    var actual = try Ir.initStrExpr("(+ 1 2)", std.testing.allocator);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&Ir{ .function_call = .{
        .function = @constCast(&Ir{ .constant = .{ .symbol = "+" } }),
        .args = @constCast(&[_]*Ir{
            @constCast(&Ir{ .constant = .{ .int = 1 } }),
            @constCast(&Ir{ .constant = .{ .int = 2 } }),
        }),
    } }, actual);
}

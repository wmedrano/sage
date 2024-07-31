const std = @import("std");

const Ast = @import("ast.zig").Ast;
const AstCollection = @import("ast.zig").AstCollection;
const AstType = @import("ast.zig").AstType;
const ObjectManager = @import("object_manager.zig").ObjectManager;
const Ir = @import("ir.zig").Ir;
const Leaf = @import("ast.zig").Leaf;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Val = @import("val.zig").Val;

pub const ByteCode = union(enum) {
    /// Push a constant to the stack. Contains the index to the constant within the constants array.
    push_const: usize,
    /// Replace the top symbol value in the stack with its real value. Contains the index to the
    /// constant within the constants array. The constant must be a symbol.
    deref: usize,
    /// Get the nth argument.
    get_arg: usize,
    /// Evaluate the top n elements in the stack. The deepest element should contain a function with
    /// the rest of the elements containing the arguments.
    eval: usize,
    /// Jump by usize.
    jump: usize,
    /// Jump by usize if the top value of the stack is true.
    jump_if: usize,
    /// Return from the current function.
    ///   - Take the top value of the stack as the return value.
    ///   - Pop all items on the stack from the function frame's start to the end.
    ///   - Push the return value to the top of the stack.
    ///   - Pop the function frame.
    ret: void,

    /// Pretty print the instruction.
    pub fn format(self: *const ByteCode, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            .push_const => |v| try writer.print("push_const({any})", .{v}),
            .deref => |n| try writer.print("deref({any})", .{n}),
            .get_arg => |n| try writer.print("get_arg({})", .{n}),
            .eval => |n| try writer.print("eval({})", .{n}),
            .jump => |n| try writer.print("jump({})", .{n}),
            .jump_if => |n| try writer.print("jump_if({})", .{n}),
            .ret => try writer.print("return", .{}),
        }
    }
};

pub const ByteCodeFunc = struct {
    /// Contains the sequence of instructions to run.
    instructions: []ByteCode,
    /// Contains all constants. These are referenced by index.
    constants: []Val,

    /// Create a new ByteCodeFunc from an Ir.
    pub fn init(ir: *const Ir, object_manager: *ObjectManager) !ByteCodeFunc {
        var instructions = std.ArrayListUnmanaged(ByteCode){};
        errdefer instructions.deinit(object_manager.allocator);
        var constants = std.ArrayListUnmanaged(Val){};
        errdefer constants.deinit(object_manager.allocator);
        try ByteCodeFunc.initImpl(ir, &instructions, &constants, object_manager);
        try instructions.append(object_manager.allocator, .ret);
        return .{
            .instructions = try instructions.toOwnedSlice(object_manager.allocator),
            .constants = try constants.toOwnedSlice(object_manager.allocator),
        };
    }

    /// Create a new ByteCodeFunc from a string expression.
    pub fn initStrExpr(expr: []const u8, object_manager: *ObjectManager) !ByteCodeFunc {
        const ir = try Ir.initStrExpr(expr, object_manager);
        defer ir.deinit(object_manager.allocator);
        return ByteCodeFunc.init(ir, object_manager);
    }

    /// Deallocate all memory associated with the ByteCodeFunc.
    pub fn deinit(self: *ByteCodeFunc, allocator: std.mem.Allocator) void {
        allocator.free(self.constants);
        allocator.free(self.instructions);
    }

    /// Pretty print the bytecode instructions.
    pub fn format(self: *const ByteCodeFunc, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Instructions:\n", .{});
        for (0.., self.instructions) |idx, instruction| {
            try writer.print("  {d}: {any}\n", .{ idx, instruction });
        }

        try writer.print("Constants:\n", .{});
        for (0.., self.constants) |idx, constant| {
            try writer.print("  {d}: {any}\n", .{ idx, constant });
        }
    }

    fn initImpl(ir: *const Ir, res: *std.ArrayListUnmanaged(ByteCode), constants: *std.ArrayListUnmanaged(Val), object_manager: *ObjectManager) !void {
        switch (ir.*) {
            .constant => |val| {
                const val_idx = constants.items.len;
                try res.append(object_manager.allocator, .{ .push_const = val_idx });
                try constants.append(object_manager.allocator, val);
            },
            .deref => |d| {
                const val_idx = constants.items.len;
                const symbol_val = try object_manager.allocGlobalSymbol(d);
                try constants.append(object_manager.allocator, symbol_val);
                try res.append(object_manager.allocator, .{ .deref = val_idx });
            },
            .get_arg => |idx| try res.append(object_manager.allocator, .{ .get_arg = idx }),
            .function_call => |f| {
                try ByteCodeFunc.initImpl(f.function, res, constants, object_manager);
                for (f.args) |a| try ByteCodeFunc.initImpl(a, res, constants, object_manager);
                try res.append(object_manager.allocator, .{ .eval = f.args.len + 1 });
            },
            .if_expr => |expr| {
                try initImpl(expr.predicate, res, constants, object_manager);
                // True branch
                var true_expr_res = std.ArrayListUnmanaged(ByteCode){};
                defer true_expr_res.deinit(object_manager.allocator);
                try initImpl(expr.true_expr, &true_expr_res, constants, object_manager);
                // False branch
                var false_expr_res = std.ArrayListUnmanaged(ByteCode){};
                defer false_expr_res.deinit(object_manager.allocator);
                try initImpl(if (expr.false_expr) |f| f else &Ir{ .constant = .void }, &false_expr_res, constants, object_manager);
                // Make final expression.
                try res.append(object_manager.allocator, .{ .jump_if = false_expr_res.items.len + 1 });
                try res.appendSlice(object_manager.allocator, false_expr_res.items);
                try res.append(object_manager.allocator, .{ .jump = true_expr_res.items.len });
                try res.appendSlice(object_manager.allocator, true_expr_res.items);
            },
            .lambda => |lambda| {
                var l_instructions = std.ArrayListUnmanaged(ByteCode){};
                errdefer l_instructions.deinit(object_manager.allocator);
                var l_constants = std.ArrayListUnmanaged(Val){};
                errdefer l_constants.deinit(object_manager.allocator);
                for (lambda.exprs) |expr| {
                    try ByteCodeFunc.initImpl(expr, &l_instructions, &l_constants, object_manager);
                }
                try l_instructions.append(object_manager.allocator, .ret);
                const lambda_fn = try object_manager.allocFunction();
                lambda_fn.* = .{
                    .name = try object_manager.allocator.dupe(u8, lambda.name),
                    .is_static = false,
                    .function = .{
                        .bytecode = .{
                            .instructions = try l_instructions.toOwnedSlice(object_manager.allocator),
                            .constants = try l_constants.toOwnedSlice(object_manager.allocator),
                        },
                    },
                };
                const lambda_ir = Ir{
                    .constant = .{
                        .function = lambda_fn,
                    },
                };
                try initImpl(&lambda_ir, res, constants, object_manager);
            },
        }
    }
};

test "bytecode size is small" {
    try std.testing.expectEqual(2 * @sizeOf(usize), @sizeOf(ByteCode));
}

test "push single value" {
    var object_manager = ObjectManager.init(std.testing.allocator);
    defer object_manager.deinit();
    var actual = try ByteCodeFunc.initStrExpr("1", &object_manager);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqualDeep(&[_]Val{
        .{ .int = 1 },
    }, actual.constants);
}

test "simple expression" {
    var object_manager = ObjectManager.init(std.testing.allocator);
    defer object_manager.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(+ 1 2 variable)", &object_manager);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .deref = 0 },
        .{ .push_const = 1 },
        .{ .push_const = 2 },
        .{ .deref = 3 },
        .{ .eval = 4 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqualDeep(&[_]Val{
        try object_manager.allocGlobalSymbol("+"),
        .{ .int = 1 },
        .{ .int = 2 },
        try object_manager.allocGlobalSymbol("variable"),
    }, actual.constants);
}

test "if statement" {
    var object_manager = ObjectManager.init(std.testing.allocator);
    defer object_manager.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(if true 1 2)", &object_manager);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .{ .jump_if = 2 },
        .{ .push_const = 2 },
        .{ .jump = 1 },
        .{ .push_const = 1 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqualDeep(&[_]Val{
        .{ .boolean = true },
        .{ .int = 1 },
        .{ .int = 2 },
    }, actual.constants);
}

test "if statement without false branch uses void false branch" {
    var object_manager = ObjectManager.init(std.testing.allocator);
    defer object_manager.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(if true 1)", &object_manager);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .{ .jump_if = 2 },
        .{ .push_const = 2 },
        .{ .jump = 1 },
        .{ .push_const = 1 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqualDeep(&[_]Val{
        .{ .boolean = true },
        .{ .int = 1 },
        .void,
    }, actual.constants);
}

test "lambda is pushed" {
    var object_manager = ObjectManager.init(std.testing.allocator);
    defer object_manager.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(lambda (a b) (+ a b))", &object_manager);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqual(1, actual.constants.len);
}

test "recursive function" {
    var object_manager = ObjectManager.init(std.testing.allocator);
    defer object_manager.deinit();
    var actual = try ByteCodeFunc.initStrExpr(
        "(define fib (lambda (n) (if (< n 1) 0 (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))))",
        &object_manager,
    );
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .deref = 0 },
        .{ .push_const = 1 },
        .{ .push_const = 2 },
        .{ .eval = 3 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqual(1, object_manager.global_functions.items.len);
    try std.testing.expectEqualDeep(&[_]Val{
        try object_manager.allocGlobalSymbol("%define"),
        try object_manager.allocGlobalSymbol("fib"),
        .{ .function = object_manager.global_functions.items[0] },
    }, actual.constants);
    try std.testing.expectEqualDeep(
        object_manager.global_functions.items[0].name,
        "fib",
    );
}

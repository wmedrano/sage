const std = @import("std");

const Ast = @import("ast.zig").Ast;
const AstCollection = @import("ast.zig").AstCollection;
const AstType = @import("ast.zig").AstType;
const Heap = @import("heap.zig").Heap;
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
    pub fn init(ir: *const Ir, heap: *Heap) !ByteCodeFunc {
        var instructions = std.ArrayListUnmanaged(ByteCode){};
        errdefer instructions.deinit(heap.allocator);
        var constants = std.ArrayListUnmanaged(Val){};
        errdefer constants.deinit(heap.allocator);
        try ByteCodeFunc.initImpl(ir, &instructions, &constants, heap);
        try instructions.append(heap.allocator, .ret);
        return .{
            .instructions = try instructions.toOwnedSlice(heap.allocator),
            .constants = try constants.toOwnedSlice(heap.allocator),
        };
    }

    /// Create a new ByteCodeFunc from a string expression.
    pub fn initStrExpr(expr: []const u8, heap: *Heap) !ByteCodeFunc {
        const ir = try Ir.initStrExpr(expr, heap);
        defer ir.deinit(heap.allocator);
        return ByteCodeFunc.init(ir, heap);
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

    fn initImpl(ir: *const Ir, res: *std.ArrayListUnmanaged(ByteCode), constants: *std.ArrayListUnmanaged(Val), heap: *Heap) !void {
        switch (ir.*) {
            .constant => |val| {
                const val_idx = constants.items.len;
                try res.append(heap.allocator, .{ .push_const = val_idx });
                try constants.append(heap.allocator, val);
            },
            .deref => |d| {
                const val_idx = constants.items.len;
                const symbol_val = try heap.allocGlobalSymbol(d);
                try constants.append(heap.allocator, symbol_val);
                try res.append(heap.allocator, .{ .deref = val_idx });
            },
            .get_arg => |idx| try res.append(heap.allocator, .{ .get_arg = idx }),
            .function_call => |f| {
                try ByteCodeFunc.initImpl(f.function, res, constants, heap);
                for (f.args) |a| try ByteCodeFunc.initImpl(a, res, constants, heap);
                try res.append(heap.allocator, .{ .eval = f.args.len + 1 });
            },
            .if_expr => |expr| {
                try initImpl(expr.predicate, res, constants, heap);
                // True branch
                var true_expr_res = std.ArrayListUnmanaged(ByteCode){};
                defer true_expr_res.deinit(heap.allocator);
                try initImpl(expr.true_expr, &true_expr_res, constants, heap);
                // False branch
                var false_expr_res = std.ArrayListUnmanaged(ByteCode){};
                defer false_expr_res.deinit(heap.allocator);
                try initImpl(if (expr.false_expr) |f| f else &Ir{ .constant = .void }, &false_expr_res, constants, heap);
                // Make final expression.
                try res.append(heap.allocator, .{ .jump_if = false_expr_res.items.len + 1 });
                try res.appendSlice(heap.allocator, false_expr_res.items);
                try res.append(heap.allocator, .{ .jump = true_expr_res.items.len });
                try res.appendSlice(heap.allocator, true_expr_res.items);
            },
            .lambda => |lambda| {
                var l_instructions = std.ArrayListUnmanaged(ByteCode){};
                errdefer l_instructions.deinit(heap.allocator);
                var l_constants = std.ArrayListUnmanaged(Val){};
                errdefer l_constants.deinit(heap.allocator);
                for (lambda.exprs) |expr| {
                    try ByteCodeFunc.initImpl(expr, &l_instructions, &l_constants, heap);
                }
                try l_instructions.append(heap.allocator, .ret);
                const lambda_fn = try heap.allocFunction();
                lambda_fn.* = .{
                    .name = try heap.allocator.dupe(u8, lambda.name),
                    .is_static = false,
                    .function = .{
                        .bytecode = .{
                            .instructions = try l_instructions.toOwnedSlice(heap.allocator),
                            .constants = try l_constants.toOwnedSlice(heap.allocator),
                        },
                    },
                };
                const lambda_ir = Ir{
                    .constant = .{
                        .function = lambda_fn,
                    },
                };
                try initImpl(&lambda_ir, res, constants, heap);
            },
        }
    }
};

test "bytecode size is small" {
    try std.testing.expectEqual(2 * @sizeOf(usize), @sizeOf(ByteCode));
}

test "push single value" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr("1", &heap);
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
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(+ 1 2 variable)", &heap);
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
        try heap.allocGlobalSymbol("+"),
        .{ .int = 1 },
        .{ .int = 2 },
        try heap.allocGlobalSymbol("variable"),
    }, actual.constants);
}

test "if statement" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(if true 1 2)", &heap);
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
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(if true 1)", &heap);
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
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(lambda (a b) (+ a b))", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqual(1, actual.constants.len);
}

test "recursive function" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr(
        "(define fib (lambda (n) (if (< n 1) 0 (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))))",
        &heap,
    );
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .deref = 0 },
        .{ .push_const = 1 },
        .{ .push_const = 2 },
        .{ .eval = 3 },
        .ret,
    }, actual.instructions);
    try std.testing.expectEqual(1, heap.global_functions.items.len);
    try std.testing.expectEqualDeep(&[_]Val{
        try heap.allocGlobalSymbol("%define"),
        try heap.allocGlobalSymbol("fib"),
        .{ .function = heap.global_functions.items[0] },
    }, actual.constants);
    try std.testing.expectEqualDeep(
        heap.global_functions.items[0].name,
        "fib",
    );
}

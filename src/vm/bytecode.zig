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
    /// Replace the top symbol value in the stack with its real value.
    deref,
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
            .deref => try writer.print("deref", .{}),
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
    instructions: std.ArrayListUnmanaged(ByteCode),
    /// Contains all constants. These are referenced by index.
    constants: std.ArrayListUnmanaged(Val),

    /// Create a new ByteCodeFunc from an Ir.
    pub fn init(ir: *const Ir, heap: *Heap) !ByteCodeFunc {
        var instructions = std.ArrayListUnmanaged(ByteCode){};
        var constants = std.ArrayListUnmanaged(Val){};
        try ByteCodeFunc.initImpl(ir, &instructions, &constants, heap);
        try instructions.append(heap.allocator, .ret);
        return .{
            .instructions = instructions,
            .constants = constants,
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
        self.constants.deinit(allocator);
        self.instructions.deinit(allocator);
    }

    /// Pretty print the bytecode instructions.
    pub fn format(self: *const ByteCodeFunc, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Instructions:\n", .{});
        for (0.., self.instructions.items) |idx, instruction| {
            try writer.print("  {d}: {any}\n", .{ idx, instruction });
        }

        try writer.print("Constants:\n", .{});
        for (0.., self.constants.items) |idx, constant| {
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
                try res.appendSlice(heap.allocator, &[_]ByteCode{
                    .{ .push_const = val_idx },
                    .deref,
                });
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
                var instructions = std.ArrayListUnmanaged(ByteCode){};
                var l_constants = std.ArrayListUnmanaged(Val){};
                for (lambda.exprs) |expr| {
                    try ByteCodeFunc.initImpl(expr, &instructions, &l_constants, heap);
                }
                try instructions.append(heap.allocator, .ret);
                const lambda_fn = try heap.allocFunction();
                lambda_fn.* = .{
                    .name = "",
                    .is_static = false,
                    .function = .{
                        .bytecode = .{
                            .instructions = instructions,
                            .constants = l_constants,
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

fn leafToVal(l: *const Leaf, alloc: std.mem.Allocator) !Val {
    switch (l.*) {
        Leaf.identifier => return .{ .symbol = l.identifier },
        Leaf.string => return try Val.initStrExpring(l.string, alloc),
        Leaf.int => return .{ .int = l.int },
        Leaf.float => return .{ .float = l.float },
    }
}

fn deinitArrayList(comptime T: type, arr: *std.ArrayList(T)) void {
    for (arr.items) |x| x.deinit(arr.allocator);
    arr.deinit();
}

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
    }, actual.instructions.items);
    try std.testing.expectEqualDeep(&[_]Val{
        .{ .int = 1 },
    }, actual.constants.items);
}

test "simple expression" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(+ 1 2 variable)", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .deref,
        .{ .push_const = 1 },
        .{ .push_const = 2 },
        .{ .push_const = 3 },
        .deref,
        .{ .eval = 4 },
        .ret,
    }, actual.instructions.items);
    try std.testing.expectEqualDeep(&[_]Val{
        try heap.allocGlobalSymbol("+"),
        .{ .int = 1 },
        .{ .int = 2 },
        try heap.allocGlobalSymbol("variable"),
    }, actual.constants.items);
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
    }, actual.instructions.items);
    try std.testing.expectEqualDeep(&[_]Val{
        .{ .boolean = true },
        .{ .int = 1 },
        .{ .int = 2 },
    }, actual.constants.items);
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
    }, actual.instructions.items);
    try std.testing.expectEqualDeep(&[_]Val{
        .{ .boolean = true },
        .{ .int = 1 },
        .void,
    }, actual.constants.items);
}

test "lambda is pushed" {
    var heap = Heap.init(std.testing.allocator);
    defer heap.deinit();
    var actual = try ByteCodeFunc.initStrExpr("(lambda (a b) (+ a b))", &heap);
    defer actual.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .ret,
    }, actual.instructions.items);
    try std.testing.expectEqual(1, actual.constants.items.len);
}

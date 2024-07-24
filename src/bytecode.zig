const std = @import("std");

const Ir = @import("ir.zig").Ir;
const IrType = @import("ir.zig").IrType;
const Ast = @import("ast.zig").Ast;
const AstCollection = @import("ast.zig").AstCollection;
const AstType = @import("ast.zig").AstType;
const Leaf = @import("ast.zig").Leaf;
const LeafType = @import("ast.zig").LeafType;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Val = @import("val.zig").Val;
const ValType = @import("val.zig").ValType;

/// The instruction to run.
pub const ByteCodeType = enum {
    /// Push a constant to the stack.
    push_const,
    /// Pop the top item in the stack and replace it to the variable it references. The top item
    /// must by a symbol Val.
    deref,
    /// Evaluate the top N items in the stack as a function.
    eval,
    /// Return from the current bytecode frame.
    ret,
};

pub const ByteCode = union(ByteCodeType) {
    /// Push a constant to the stack. Contains the index to the constant within the constants array.
    push_const: usize,
    /// Replace the top symbol value in the stack with its real value.
    deref,
    /// Evaluate the top n elements in the stack. The deepest element should contain a function with
    /// the rest of the elements containing the arguments.
    eval: usize,
    /// Return from the current function.
    ///   - Take the top value of the stack as the return value.
    ///   - Pop all items on the stack from the function frame's start to the end.
    ///   - Push the return value to the top of the stack.
    ///   - Pop the function frame.
    ret: void,

    /// Pretty print the instruction.
    pub fn format(self: *const ByteCode, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            ByteCodeType.push_const => |v| try writer.print("push_const({any})", .{v}),
            ByteCodeType.deref => try writer.print("deref", .{}),
            ByteCodeType.eval => |n| try writer.print("eval({})", .{n}),
            ByteCodeType.ret => try writer.print("return", .{}),
        }
    }
};

pub const ByteCodeFunc = struct {
    /// Contains the sequence of instructions to run.
    instructions: std.ArrayList(ByteCode),
    /// Contains all constants. These are referenced by index.
    constants: std.ArrayListUnmanaged(Val),

    /// Create a new ByteCodeFunc from an Ir.
    pub fn init(ir: *const Ir, alloc: std.mem.Allocator) !ByteCodeFunc {
        var instructions = std.ArrayList(ByteCode).init(alloc);
        var constants = std.ArrayListUnmanaged(Val){};
        try ByteCodeFunc.initImpl(ir, &instructions, &constants);
        try instructions.append(.ret);
        return .{
            .instructions = instructions,
            .constants = constants,
        };
    }

    /// Create a new ByteCodeFunc from a string expression.
    pub fn initStrExpr(expr: []const u8, alloc: std.mem.Allocator) !ByteCodeFunc {
        const ir = try Ir.initStrExpr(expr, alloc);
        defer ir.deinit(alloc);
        return ByteCodeFunc.init(ir, alloc);
    }

    /// Deallocate all memory associated with the ByteCodeFunc.
    pub fn deinit(self: *ByteCodeFunc) void {
        for (self.constants.items) |v| v.deinit(self.instructions.allocator);
        self.constants.deinit(self.instructions.allocator);
        self.instructions.deinit();
    }

    /// Pretty print the bytecode instructions.
    pub fn format(self: *const ByteCodeFunc, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (0.., self.instructions.items) |idx, instruction| {
            try writer.print("{d}: {any}\n", .{ idx, instruction });
        }
    }

    fn initImpl(ir: *const Ir, res: *std.ArrayList(ByteCode), constants: *std.ArrayListUnmanaged(Val)) !void {
        switch (ir.*) {
            IrType.constant => |v| {
                const val = try v.clone(res.allocator);
                const val_idx = constants.items.len;
                try res.append(.{ .push_const = val_idx });
                try constants.append(res.allocator, val);
                if (@as(ValType, val) == ValType.symbol) {
                    try res.append(.deref);
                }
            },
            IrType.function_call => |f| {
                try ByteCodeFunc.initImpl(f.function, res, constants);
                for (f.args) |a| try ByteCodeFunc.initImpl(a, res, constants);
                try res.append(.{ .eval = f.args.len + 1 });
            },
        }
    }
};

fn leafToVal(l: *const Leaf, alloc: std.mem.Allocator) !Val {
    switch (l.*) {
        LeafType.identifier => return .{ .symbol = l.identifier },
        LeafType.string => return try Val.initStrExpring(l.string, alloc),
        LeafType.int => return .{ .int = l.int },
        LeafType.float => return .{ .float = l.float },
    }
}

test "push single value" {
    var actual = try ByteCodeFunc.initStrExpr("1", std.testing.allocator);
    defer actual.deinit();
    try std.testing.expectEqualDeep(&[_]ByteCode{
        .{ .push_const = 0 },
        .ret,
    }, actual.instructions.items);
    try std.testing.expectEqualDeep(&[_]Val{
        .{ .int = 1 },
    }, actual.constants.items);
}

test "evaluate expression" {
    var actual = try ByteCodeFunc.initStrExpr("(+ 1 2 variable)", std.testing.allocator);
    defer actual.deinit();
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
        .{ .symbol = "+" },
        .{ .int = 1 },
        .{ .int = 2 },
        .{ .symbol = "variable" },
    }, actual.constants.items);
}

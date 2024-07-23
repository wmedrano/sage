const std = @import("std");

const Ast = @import("ast.zig").Ast;
const AstCollection = @import("ast.zig").AstCollection;
const AstType = @import("ast.zig").AstType;
const Leaf = @import("ast.zig").Leaf;
const LeafType = @import("ast.zig").LeafType;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Val = @import("val.zig").Val;
const ValType = @import("val.zig").ValType;

pub const ByteCodeType = enum {
    push_const,
    deref,
    eval,
    ret,
};

pub const ByteCode = union(ByteCodeType) {
    push_const: usize,
    deref,
    eval: usize,
    ret: void,

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
    instructions: std.ArrayList(ByteCode),
    constants: std.ArrayList(Val),

    pub fn init(ast: *const Ast, alloc: std.mem.Allocator) !ByteCodeFunc {
        var instructions = std.ArrayList(ByteCode).init(alloc);
        var constants = std.ArrayList(Val).init(alloc);
        try ByteCodeFunc.init_impl(ast, &instructions, &constants);
        try instructions.append(.ret);
        return .{
            .instructions = instructions,
            .constants = constants,
        };
    }

    pub fn deinit(self: ByteCodeFunc) void {
        self.instructions.deinit();
        for (self.constants.items) |v| v.deinit(self.constants.allocator);
        self.constants.deinit();
    }

    pub fn format(self: *const ByteCodeFunc, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (0.., self.instructions.items) |idx, instruction| {
            try writer.print("{d}: {any}\n", .{ idx, instruction });
        }
    }

    fn init_impl(ast: *const Ast, res: *std.ArrayList(ByteCode), constants: *std.ArrayList(Val)) !void {
        switch (ast.*) {
            AstType.leaf => |*l| {
                const val = try leaf_to_val(l, res.allocator);
                const val_idx = constants.items.len;
                try res.append(.{ .push_const = val_idx });
                try constants.append(val);
                if (@as(ValType, val) == ValType.symbol) {
                    try res.append(.deref);
                }
            },
            AstType.tree => |asts| {
                for (asts) |*a| try ByteCodeFunc.init_impl(a, res, constants);
                try res.append(.{ .eval = asts.len });
            },
        }
    }
};

fn leaf_to_val(l: *const Leaf, alloc: std.mem.Allocator) !Val {
    switch (l.*) {
        LeafType.identifier => return .{ .symbol = l.identifier },
        LeafType.string => return try Val.init_string(l.string, alloc),
        LeafType.int => return .{ .int = l.int },
        LeafType.float => return .{ .float = l.float },
    }
}

test "push single value" {
    var t = Tokenizer.init("1");
    const asts = try AstCollection.init(&t, std.testing.allocator);
    defer asts.deinit();
    try std.testing.expectEqual(1, asts.asts.len);

    var actual = try ByteCodeFunc.init(&asts.asts[0], std.testing.allocator);
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
    var t = Tokenizer.init("(+ 1 2 variable)");
    const asts = try AstCollection.init(&t, std.testing.allocator);
    defer asts.deinit();
    try std.testing.expectEqual(1, asts.asts.len);

    var actual = try ByteCodeFunc.init(&asts.asts[0], std.testing.allocator);
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

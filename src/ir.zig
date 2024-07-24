const std = @import("std");
const Val = @import("val.zig").Val;
const Ast = @import("ast.zig").Ast;
const AstCollection = @import("ast.zig").AstCollection;
const AstType = @import("ast.zig").AstType;
const Leaf = @import("ast.zig").Leaf;
const LeafType = @import("ast.zig").LeafType;

pub const IrType = enum {
    constant,
    function_call,
};

pub const Ir = union(IrType) {
    constant: Val,
    function_call: struct {
        function: *Ir,
        args: []*Ir,
    },

    pub const Error = std.mem.Allocator.Error || error{ EmptyFunctionCall, TooManyExprs, EmptyExpr };

    pub fn initStrExpr(expr: []const u8, alloc: std.mem.Allocator) !*Ir {
        var asts = try AstCollection.initWithStr(expr, alloc);
        defer asts.deinit();
        if (asts.asts.len == 0) return Error.EmptyExpr;
        if (asts.asts.len > 1) return Error.TooManyExprs;
        return Ir.init(&asts.asts[0], alloc);
    }

    pub fn init(ast: *const Ast, alloc: std.mem.Allocator) !*Ir {
        switch (ast.*) {
            AstType.leaf => return Ir.initConstant(&ast.leaf, alloc),
            AstType.tree => |asts| return Ir.initFunction(asts, alloc),
        }
    }

    pub fn initConstant(leaf: *const Leaf, alloc: std.mem.Allocator) Error!*Ir {
        const v = switch (leaf.*) {
            LeafType.identifier => Val{ .symbol = leaf.identifier },
            LeafType.string => try Val.initStrExpring(leaf.string, alloc),
            LeafType.int => Val{ .int = leaf.int },
            LeafType.float => Val{ .float = leaf.float },
        };
        const ret = try alloc.create(Ir);
        ret.* = .{ .constant = v };
        return ret;
    }

    pub fn initFunction(asts: []const Ast, alloc: std.mem.Allocator) Error!*Ir {
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

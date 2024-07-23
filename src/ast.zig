const std = @import("std");
const tokenizer = @import("tokenizer.zig");

const SyntaxError = error{
    UnclosedParenthesis,
    UnmatchedCloseParenthesis,
    OutOfMemory,
};

pub const AstType = enum {
    leaf,
    tree,
};

pub const LeafType = enum {
    identifier,
    string,
    int,
    float,
};

pub const Leaf = union(LeafType) {
    identifier: []const u8,
    string: []const u8,
    int: i64,
    float: f64,

    pub fn format(self: *const Leaf, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            LeafType.identifier => |s| try writer.print("identifier({s})", .{s}),
            LeafType.string => |s| try writer.print("string({s})", .{s}),
            LeafType.int => |n| try writer.print("int({})", .{n}),
            LeafType.float => |n| try writer.print("float({})", .{n}),
        }
    }

    // Parse a leaf from an identifier. If the identifier matches a number, then it is parsed into
    // an int or float Leaf.
    pub fn from_identifier(ident: []const u8) Leaf {
        if (std.fmt.parseInt(i64, ident, 10)) |i| {
            return .{ .int = i };
        } else |_| {}
        if (std.fmt.parseFloat(f64, ident)) |f| {
            return .{ .float = f };
        } else |_| {}
        return .{ .identifier = ident };
    }
};

pub const Ast = union(AstType) {
    leaf: Leaf,
    tree: []const Ast,

    pub fn format(self: *const Ast, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return self.format_impl(0, writer);
    }

    fn format_impl(self: *const Ast, indent: u8, writer: anytype) !void {
        switch (self.*) {
            AstType.leaf => |l| {
                for (0..indent) |_| {
                    try writer.print("  ", .{});
                }
                try writer.print("{any}\n", .{l});
            },
            AstType.tree => |elements| {
                for (0.., elements) |idx, e| {
                    const new_indent = if (idx == 0) indent else indent + 1;
                    try e.format_impl(new_indent, writer);
                }
            },
        }
    }
};

pub const AstCollection = struct {
    asts: []const Ast,
    alloc: std.mem.Allocator,

    // Creates a new Ast. Some fields may reference data returned by the tokenizer. Other items
    // will use alloc to allocate memory.
    pub fn init(t: *tokenizer.Tokenizer, alloc: std.mem.Allocator) SyntaxError!AstCollection {
        const asts = try AstCollection.init_impl(t, false, alloc);
        return .{
            .asts = asts,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: AstCollection) void {
        for (self.asts) |*a| deinit_ast(a, self.alloc);
        self.alloc.free(self.asts);
    }

    fn init_impl(t: *tokenizer.Tokenizer, want_close: bool, alloc: std.mem.Allocator) SyntaxError![]Ast {
        var result = std.ArrayList(Ast).init(alloc);
        defer result.deinit();
        errdefer for (result.items) |*a| deinit_ast(a, alloc);
        var has_close = false;
        while (t.next()) |token| {
            switch (token.typ) {
                tokenizer.TokenType.whitespace => continue,
                tokenizer.TokenType.openParen => {
                    const sub_asts = try AstCollection.init_impl(t, true, alloc);
                    try result.append(.{ .tree = sub_asts });
                },
                tokenizer.TokenType.closeParen => {
                    if (!want_close) {
                        return SyntaxError.UnmatchedCloseParenthesis;
                    }
                    has_close = true;
                    break;
                },
                tokenizer.TokenType.identifier => {
                    try result.append(.{ .leaf = Leaf.from_identifier(token.contents) });
                },
                tokenizer.TokenType.string => {
                    const s = token.contents[1 .. token.contents.len - 1];
                    const string = Leaf{ .string = s };
                    try result.append(.{ .leaf = string });
                },
            }
        }
        if (want_close and !has_close) {
            return SyntaxError.UnclosedParenthesis;
        }
        return try result.toOwnedSlice();
    }
};

fn deinit_ast(ast: *const Ast, alloc: std.mem.Allocator) void {
    switch (ast.*) {
        AstType.leaf => {},
        AstType.tree => |tree| {
            for (tree) |*node| {
                deinit_ast(node, alloc);
            }
            alloc.free(tree);
        },
    }
}

test "basic expression is parsed" {
    var t = tokenizer.Tokenizer.init("(+ 1 2.1 (string-length \"hello\"))");
    var ast = try AstCollection.init(&t, std.testing.allocator);
    defer ast.deinit();

    try std.testing.expectEqualDeep(AstCollection{
        .asts = &[_]Ast{
            .{
                .tree = &.{
                    .{ .leaf = .{ .identifier = "+" } },
                    .{ .leaf = .{ .int = 1 } },
                    .{ .leaf = .{ .float = 2.1 } },
                    .{ .tree = &.{
                        .{ .leaf = .{ .identifier = "string-length" } },
                        .{ .leaf = .{ .string = "hello" } },
                    } },
                },
            },
        },
        .alloc = std.testing.allocator,
    }, ast);
}

test "multiple expressions can be parsed" {
    var t = tokenizer.Tokenizer.init("1 2.3 four \"five\"");
    var ast = try AstCollection.init(&t, std.testing.allocator);
    defer ast.deinit();

    try std.testing.expectEqualDeep(AstCollection{
        .asts = &[_]Ast{
            .{ .leaf = .{ .int = 1 } },
            .{ .leaf = .{ .float = 2.3 } },
            .{ .leaf = .{ .identifier = "four" } },
            .{ .leaf = .{ .string = "five" } },
        },
        .alloc = std.testing.allocator,
    }, ast);
}

test "unmatched closing brace is error" {
    var t = tokenizer.Tokenizer.init("())");
    const ast_or_err = AstCollection.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnmatchedCloseParenthesis, ast_or_err);
}

test "unmatched opening brace is error" {
    var t = tokenizer.Tokenizer.init("(()");
    const ast_or_err = AstCollection.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnclosedParenthesis, ast_or_err);
}

test "error on second expression is detected" {
    var t = tokenizer.Tokenizer.init("(+ 1 2 3) ))");
    const ast_or_err = AstCollection.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnmatchedCloseParenthesis, ast_or_err);
}

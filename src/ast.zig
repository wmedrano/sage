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

    // Creates a new Ast. Some fields may reference data returned by the tokenizer. Other items
    // will use alloc to allocate memory.
    pub fn init(t: *tokenizer.Tokenizer, alloc: std.mem.Allocator) SyntaxError![]Ast {
        return init_impl(t, false, alloc);
    }

    fn init_impl(t: *tokenizer.Tokenizer, want_close: bool, alloc: std.mem.Allocator) SyntaxError![]Ast {
        var result = std.ArrayList(Ast).init(alloc);
        defer result.deinit();
        var has_close = false;
        while (t.next()) |token| {
            switch (token.typ) {
                tokenizer.TokenType.whitespace => continue,
                tokenizer.TokenType.openParen => {
                    const sub_asts = try Ast.init_impl(t, true, alloc);
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
        const results = try alloc.alloc(Ast, result.items.len);
        std.mem.copyForwards(Ast, results, result.items);
        return results;
    }

    // Free the memory allocated by self. If you have a slice created by init, consider using
    // deinit_slice instead.
    pub fn deinit(self: *const Ast, alloc: std.mem.Allocator) void {
        switch (self.*) {
            AstType.leaf => {},
            AstType.tree => |tree| {
                for (tree) |*node| {
                    node.deinit(alloc);
                }
                alloc.free(tree);
            },
        }
    }
};

// Calls deinit on each Ast and deallocates the memory of the slice.
pub fn deinit_slice(slice: []Ast, alloc: std.mem.Allocator) void {
    for (slice) |a| a.deinit(alloc);
    alloc.free(slice);
}

test "basic expression is parsed" {
    var t = tokenizer.Tokenizer.init("(+ 1 2.1 (string-length \"hello\"))");
    const ast = try Ast.init(&t, std.testing.allocator);
    defer deinit_slice(ast, std.testing.allocator);

    try std.testing.expectEqualDeep(&[_]Ast{
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
    }, ast);
}

test "multiple expressions can be parsed" {
    var t = tokenizer.Tokenizer.init("1 2.3 four \"five\"");
    const ast = try Ast.init(&t, std.testing.allocator);
    defer deinit_slice(ast, std.testing.allocator);

    try std.testing.expectEqualDeep(&[_]Ast{
        .{ .leaf = .{ .int = 1 } },
        .{ .leaf = .{ .float = 2.3 } },
        .{ .leaf = .{ .identifier = "four" } },
        .{ .leaf = .{ .string = "five" } },
    }, ast);
}

test "unmatched closing brace is error" {
    var t = tokenizer.Tokenizer.init("())");
    const ast_or_err = Ast.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnmatchedCloseParenthesis, ast_or_err);
}

test "unmatched opening brace is error" {
    var t = tokenizer.Tokenizer.init("(()");
    const ast_or_err = Ast.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnclosedParenthesis, ast_or_err);
}

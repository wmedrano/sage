const std = @import("std");
const tokenizer = @import("tokenizer.zig");

const SyntaxError = error{
    /// An unclosed parenthesis. Example: (this-is-not-closed
    UnclosedParenthesis,
    /// There was a close parenthesis with no open parenthesis. Example: this-is-not-closed)
    UnmatchedCloseParenthesis,
    /// Ran out of memory while parsing the syntax.
    OutOfMemory,
};

/// The different types of Ast.
pub const AstType = enum {
    /// Contains a leaf node. These are usually literals like "1", 1, or one.
    leaf,
    /// A tree that can contain any number of subtrees or leafs.
    tree,
};

/// The type of leaf node.
pub const LeafType = enum {
    /// An identifier. This usually is a reference to a variable or constant.
    identifier,
    /// A string literal.
    string,
    /// An integer literal.
    int,
    /// A float literal.
    float,
};

pub const Leaf = union(LeafType) {
    /// A reference to a variable or constant. The name is stored as a string.
    identifier: []const u8,
    /// A string literal. The contents (without the literal quotes) are stored as a string.
    string: []const u8,
    /// An integer literal. The contents are parsed as an i64.
    int: i64,
    /// A float literal. The contents are parsed as an f64.
    float: f64,

    /// Pretty print the AST.
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
    pub fn fromIdentifier(ident: []const u8) Leaf {
        if (std.fmt.parseInt(i64, ident, 10)) |i| {
            return .{ .int = i };
        } else |_| {}
        if (std.fmt.parseFloat(f64, ident)) |f| {
            return .{ .float = f };
        } else |_| {}
        return .{ .identifier = ident };
    }
};

/// Contains an Abstract Syntax Tree. It itself can be a leaf node, or a tree containing any number
/// of subtrees or leaf nodes.
pub const Ast = union(AstType) {
    /// A single leaf node. This is usually a literal or reference to a variable/constant.
    leaf: Leaf,
    /// A tree. Typically a tree denotes a function call where the first item denotes the function
    /// and the proceeding items are the arguments.
    tree: []const Ast,

    /// Pretty print the AST.
    pub fn format(self: *const Ast, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return self.formatImpl(0, writer);
    }

    fn formatImpl(self: *const Ast, indent: u8, writer: anytype) !void {
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
                    try e.formatImpl(new_indent, writer);
                }
            },
        }
    }
};

/// A collection of multiple ASTs.
pub const AstCollection = struct {
    /// The ASTs.
    asts: []const Ast,
    /// The Allocator that was used to allocate the ASTs.
    alloc: std.mem.Allocator,

    // Creates a new Ast. Some fields may reference data from the tokenizer. Other items // will use
    // alloc to allocate memory.
    pub fn init(t: *tokenizer.Tokenizer, alloc: std.mem.Allocator) SyntaxError!AstCollection {
        const asts = try AstCollection.initImpl(t, false, alloc);
        return .{
            .asts = asts,
            .alloc = alloc,
        };
    }

    /// Create a new AstCollection with src as the source code. The created ASTs may reference bytes
    /// from the src string.
    pub fn initWithStr(src: []const u8, alloc: std.mem.Allocator) SyntaxError!AstCollection {
        var t = tokenizer.Tokenizer.init(src);
        return AstCollection.init(&t, alloc);
    }

    /// Deallocate all ASTs within the collection.
    pub fn deinit(self: AstCollection) void {
        for (self.asts) |*a| deinitAst(a, self.alloc);
        self.alloc.free(self.asts);
    }

    fn initImpl(t: *tokenizer.Tokenizer, want_close: bool, alloc: std.mem.Allocator) SyntaxError![]Ast {
        var result = std.ArrayList(Ast).init(alloc);
        defer result.deinit();
        errdefer for (result.items) |*a| deinitAst(a, alloc);
        var has_close = false;
        while (t.next()) |token| {
            switch (token.typ) {
                tokenizer.TokenType.whitespace => continue,
                tokenizer.TokenType.openParen => {
                    const sub_asts = try AstCollection.initImpl(t, true, alloc);
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
                    try result.append(.{ .leaf = Leaf.fromIdentifier(token.contents) });
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

fn deinitAst(ast: *const Ast, alloc: std.mem.Allocator) void {
    switch (ast.*) {
        AstType.leaf => {},
        AstType.tree => |tree| {
            for (tree) |*node| {
                deinitAst(node, alloc);
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

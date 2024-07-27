const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;

const SyntaxError = error{
    /// An unclosed parenthesis. Example: (this-is-not-closed
    UnclosedParenthesis,
    /// There was a close parenthesis with no open parenthesis. Example: this-is-not-closed)
    UnmatchedCloseParenthesis,
    /// Ran out of memory while parsing the syntax.
    OutOfMemory,
};

pub const Leaf = union(enum) {
    /// A keyword.
    keyword: enum {
        // "if"
        if_expr,
        // "lambda"
        lambda,
        // "define"
        define,
    },
    /// A reference to a variable or constant. The name is stored as a string.
    identifier: []const u8,
    /// A string literal. The contents (without the literal quotes) are stored as a string.
    string: []const u8,
    /// A boolean literal.
    boolean: bool,
    /// An integer literal. The contents are parsed as an i64.
    int: i64,
    /// A float literal. The contents are parsed as an f64.
    float: f64,

    /// Pretty print the AST.
    pub fn format(self: *const Leaf, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            .keyword => |k| switch (k) {
                .if_expr => try writer.print("if", .{}),
                .lambda => try writer.print("lambda", .{}),
                .define => try writer.print("define", .{}),
            },
            .identifier => |s| try writer.print("identifier({s})", .{s}),
            .string => |s| try writer.print("string({s})", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .int => |n| try writer.print("int({})", .{n}),
            .float => |n| try writer.print("float({})", .{n}),
        }
    }

    // Parse a leaf from an identifier. If the identifier matches a number, then it is parsed into
    // an int or float Leaf.
    pub fn fromIdentifier(ident: []const u8) Leaf {
        if (std.mem.eql(u8, "true", ident)) {
            return .{ .boolean = true };
        }
        if (std.mem.eql(u8, "false", ident)) {
            return .{ .boolean = false };
        }
        if (std.mem.eql(u8, "if", ident)) {
            return .{ .keyword = .if_expr };
        }
        if (std.mem.eql(u8, "lambda", ident)) {
            return .{ .keyword = .lambda };
        }
        if (std.mem.eql(u8, "define", ident)) {
            return .{ .keyword = .define };
        }
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
pub const Ast = union(enum) {
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
            .leaf => |l| {
                for (0..indent) |_| {
                    try writer.print("  ", .{});
                }
                try writer.print("{any}\n", .{l});
            },
            .tree => |elements| {
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
    pub fn init(t: *Tokenizer, alloc: std.mem.Allocator) SyntaxError!AstCollection {
        const asts = try AstCollection.initImpl(t, false, alloc);
        return .{
            .asts = asts,
            .alloc = alloc,
        };
    }

    /// Create a new AstCollection with src as the source code. The created ASTs may reference bytes
    /// from the src string.
    pub fn initWithStr(src: []const u8, alloc: std.mem.Allocator) SyntaxError!AstCollection {
        var t = Tokenizer.init(src);
        return AstCollection.init(&t, alloc);
    }

    /// Deallocate all ASTs within the collection.
    pub fn deinit(self: AstCollection) void {
        for (self.asts) |*a| deinitAst(a, self.alloc);
        self.alloc.free(self.asts);
    }

    fn initImpl(t: *Tokenizer, want_close: bool, alloc: std.mem.Allocator) SyntaxError![]Ast {
        var result = std.ArrayList(Ast).init(alloc);
        defer result.deinit();
        errdefer for (result.items) |*a| deinitAst(a, alloc);
        var has_close = false;
        while (t.next()) |token| {
            switch (token.typ) {
                .whitespace => continue,
                .openParen => {
                    const sub_asts = try AstCollection.initImpl(t, true, alloc);
                    try result.append(.{ .tree = sub_asts });
                },
                .closeParen => {
                    if (!want_close) {
                        return SyntaxError.UnmatchedCloseParenthesis;
                    }
                    has_close = true;
                    break;
                },
                .identifier => {
                    try result.append(.{ .leaf = Leaf.fromIdentifier(token.contents) });
                },
                .string => {
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
        Ast.leaf => {},
        Ast.tree => |tree| {
            for (tree) |*node| {
                deinitAst(node, alloc);
            }
            alloc.free(tree);
        },
    }
}

test "basic expression is parsed" {
    var t = Tokenizer.init("(define + 1 2.1 (string-length \"hello\") (if true 10) (if false 11 12))");
    var ast = try AstCollection.init(&t, std.testing.allocator);
    defer ast.deinit();

    try std.testing.expectEqualDeep(AstCollection{
        .asts = &[_]Ast{
            .{
                .tree = &.{
                    .{ .leaf = .{ .keyword = .define } },
                    .{ .leaf = .{ .identifier = "+" } },
                    .{ .leaf = .{ .int = 1 } },
                    .{ .leaf = .{ .float = 2.1 } },
                    .{ .tree = &.{
                        .{ .leaf = .{ .identifier = "string-length" } },
                        .{ .leaf = .{ .string = "hello" } },
                    } },
                    .{ .tree = &.{
                        .{ .leaf = .{ .keyword = .if_expr } },
                        .{ .leaf = .{ .boolean = true } },
                        .{ .leaf = .{ .int = 10 } },
                    } },
                    .{ .tree = &.{
                        .{ .leaf = .{ .keyword = .if_expr } },
                        .{ .leaf = .{ .boolean = false } },
                        .{ .leaf = .{ .int = 11 } },
                        .{ .leaf = .{ .int = 12 } },
                    } },
                },
            },
        },
        .alloc = std.testing.allocator,
    }, ast);
}

test "lambda is parsed" {
    var t = Tokenizer.init("(lambda (a b) (+ a b))");
    var ast = try AstCollection.init(&t, std.testing.allocator);
    defer ast.deinit();

    try std.testing.expectEqualDeep(AstCollection{
        .asts = &[_]Ast{
            .{
                .tree = &.{
                    .{ .leaf = .{ .keyword = .lambda } },
                    .{ .tree = &.{
                        .{ .leaf = .{ .identifier = "a" } },
                        .{ .leaf = .{ .identifier = "b" } },
                    } },
                    .{ .tree = &.{
                        .{ .leaf = .{ .identifier = "+" } },
                        .{ .leaf = .{ .identifier = "a" } },
                        .{ .leaf = .{ .identifier = "b" } },
                    } },
                },
            },
        },
        .alloc = std.testing.allocator,
    }, ast);
}

test "multiple expressions can be parsed" {
    var t = Tokenizer.init("1 2.3 four \"five\"");
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
    var t = Tokenizer.init("())");
    const ast_or_err = AstCollection.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnmatchedCloseParenthesis, ast_or_err);
}

test "unmatched opening brace is error" {
    var t = Tokenizer.init("(()");
    const ast_or_err = AstCollection.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnclosedParenthesis, ast_or_err);
}

test "error on second expression is detected" {
    var t = Tokenizer.init("(+ 1 2 3) ))");
    const ast_or_err = AstCollection.init(&t, std.testing.allocator);
    try std.testing.expectError(SyntaxError.UnmatchedCloseParenthesis, ast_or_err);
}

const std = @import("std");

pub const TokenType = enum {
    whitespace,
    openParen,
    closeParen,
    identifier,
    string,

    // Guess the type of s by looking at the first character.
    pub fn guessType(s: []const u8) TokenType {
        if (s.len == 1) {
            return switch (s[0]) {
                ' ' => return TokenType.whitespace,
                '\t' => return TokenType.whitespace,
                '\n' => return TokenType.whitespace,
                '(' => return TokenType.openParen,
                ')' => return TokenType.closeParen,
                '"' => return TokenType.string,
                else => return TokenType.identifier,
            };
        }
        return TokenType.identifier;
    }
};

pub const Token = struct {
    typ: TokenType,
    contents: []const u8,
};

pub const Tokenizer = struct {
    contents: []const u8,
    idx: usize,

    // Create a tokenizer over contents.
    pub fn init(contents: []const u8) Tokenizer {
        return Tokenizer{
            .contents = contents,
            .idx = 0,
        };
    }

    // Start parsing the input stream from the beginning.
    pub fn reset(self: *Tokenizer) void {
        self.idx = 0;
    }

    // Peek at the next token without advancing the iterator.
    pub fn peek(self: *Tokenizer) ?Token {
        if (self.idx == self.contents.len) {
            return null;
        }
        const start = self.idx;
        var end = start;
        var token_type = TokenType.whitespace;
        while (end < self.contents.len) {
            const codepoint_length = std.unicode.utf8ByteSequenceLength(self.contents[0]) catch break;
            const codepoint = self.contents[end .. end + codepoint_length];
            if (start == end) {
                token_type = TokenType.guessType(codepoint);
            } else {
                const new_token_type = TokenType.guessType(codepoint);
                switch (token_type) {
                    TokenType.openParen => break,
                    TokenType.closeParen => break,
                    TokenType.string => if (new_token_type == TokenType.string) {
                        end += codepoint_length;
                        break;
                    },
                    TokenType.whitespace => if (new_token_type != TokenType.whitespace) {
                        break;
                    },
                    TokenType.identifier => if (new_token_type != TokenType.identifier) {
                        break;
                    },
                }
            }
            end += codepoint_length;
        }
        return .{
            .typ = token_type,
            .contents = self.contents[start..end],
        };
    }

    // Get the next token.
    pub fn next(self: *Tokenizer) ?Token {
        const next_val = self.peek() orelse return null;
        self.idx += next_val.contents.len;
        return next_val;
    }

    // Collect all the tokens into an AraryList.
    pub fn collectAll(self: *Tokenizer, alloc: std.mem.Allocator) std.mem.Allocator.Error!std.ArrayList(Token) {
        var ret = std.ArrayList(Token).init(alloc);
        errdefer ret.deinit();
        while (self.next()) |token| {
            try ret.append(token);
        }
        return ret;
    }
};

test "parse expression" {
    var tokenizer = Tokenizer.init(" (parse-expression-1  234)");
    const result = try tokenizer.collectAll(std.testing.allocator);
    defer result.deinit();
    try std.testing.expectEqualDeep(&[_]Token{
        .{ .typ = TokenType.whitespace, .contents = " " },
        .{ .typ = TokenType.openParen, .contents = "(" },
        .{ .typ = TokenType.identifier, .contents = "parse-expression-1" },
        .{ .typ = TokenType.whitespace, .contents = "  " },
        .{ .typ = TokenType.identifier, .contents = "234" },
        .{ .typ = TokenType.closeParen, .contents = ")" },
    }, result.items);
}

test "parse with duplicate tokens" {
    var tokenizer = Tokenizer.init("  \t\n(())\"\"");
    const result = try tokenizer.collectAll(std.testing.allocator);
    defer result.deinit();
    try std.testing.expectEqualDeep(&[_]Token{
        .{ .typ = TokenType.whitespace, .contents = "  \t\n" },
        .{ .typ = TokenType.openParen, .contents = "(" },
        .{ .typ = TokenType.openParen, .contents = "(" },
        .{ .typ = TokenType.closeParen, .contents = ")" },
        .{ .typ = TokenType.closeParen, .contents = ")" },
        .{ .typ = TokenType.string, .contents = "\"\"" },
    }, result.items);
}

test "parse string" {
    var tokenizer = Tokenizer.init("(\"(this is a string)\")");
    const result = try tokenizer.collectAll(std.testing.allocator);
    defer result.deinit();
    try std.testing.expectEqualDeep(&[_]Token{
        .{ .typ = TokenType.openParen, .contents = "(" },
        .{ .typ = TokenType.string, .contents = "\"(this is a string)\"" },
        .{ .typ = TokenType.closeParen, .contents = ")" },
    }, result.items);
}

test "empty expression" {
    var tokenizer = Tokenizer.init("");
    const result = try tokenizer.collectAll(std.testing.allocator);
    defer result.deinit();
    try std.testing.expectEqualDeep(&[_]Token{}, result.items);
}

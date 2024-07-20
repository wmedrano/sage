const std = @import("std");

pub const TokenType = enum {
    whitespace,
    openParen,
    closeParen,
    identifierentifier,

    pub fn guess_type(s: []const u8) TokenType {
        if (s.len == 1) {
            return switch (s[0]) {
                ' ' => return TokenType.whitespace,
                '\n' => return TokenType.whitespace,
                '(' => return TokenType.openParen,
                ')' => return TokenType.closeParen,
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

    pub fn init(contents: []const u8) Tokenizer {
        return Tokenizer{
            .contents = contents,
            .idx = 0,
        };
    }

    pub fn peek(self: *Tokenizer) !?Token {
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
                token_type = TokenType.guess_type(codepoint);
            } else {
                const new_token_type = TokenType.guess_type(codepoint);
                if (token_type != new_token_type) {
                    break;
                }
            }
            end += codepoint.len;
        }
        return .{
            .typ = token_type,
            .contents = self.contents[start..end],
        };
    }

    pub fn next(self: *Tokenizer) !?Token {
        const next_val = try self.peek() orelse return null;
        self.idx += next_val.contents.len;
        return next_val;
    }

    pub fn collect_all(self: *Tokenizer, alloc: std.mem.Allocator) !std.ArrayList(Token) {
        var ret = std.ArrayList(Token).init(alloc);
        errdefer ret.deinit();
        while (try self.next()) |token| {
            try ret.append(token);
        }
        return ret;
    }
};

test "parse expression" {
    var tokenizer = Tokenizer.init(" (parse-expression-1  234)");
    const result = try tokenizer.collect_all(std.testing.allocator);
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

test "empty expression" {
    var tokenizer = Tokenizer.init("");
    const result = try tokenizer.collect_all(std.testing.allocator);
    defer result.deinit();
    try std.testing.expectEqualDeep(&[_]Token{}, result.items);
}
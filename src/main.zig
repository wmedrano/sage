const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    defer bw.flush() catch {};

    const filename = "main.sage";
    const max_file_size = 1024 * 1024 * 1024; // 1 GiB
    const contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, max_file_size);
    var tokenizer = Tokenizer.init(contents);

    while (try tokenizer.next()) |token| {
        try bw.writer().print("Token(len={any}): {s}\n", .{ token.len, token });
        try bw.flush();
    }
}

const TokenType = enum {
    openParen,
    closeParen,
    whitespace,
    word,

    pub fn guess_type(s: []const u8) TokenType {
        if (s.len == 1) {
            return switch (s[0]) {
                ' ' => return TokenType.whitespace,
                '\n' => return TokenType.whitespace,
                '(' => return TokenType.openParen,
                ')' => return TokenType.closeParen,
                else => return TokenType.word,
            };
        }
        return TokenType.word;
    }
};

const Tokenizer = struct {
    contents: []const u8,
    idx: usize,

    pub fn init(contents: []const u8) Tokenizer {
        return Tokenizer{
            .contents = contents,
            .idx = 0,
        };
    }

    pub fn peek(self: *Tokenizer) !?[]const u8 {
        std.time.sleep(200000000);
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
        return self.contents[start..end];
    }

    pub fn next(self: *Tokenizer) !?[]const u8 {
        const next_val = try self.peek() orelse return null;
        self.idx += next_val.len;
        return next_val;
    }
};

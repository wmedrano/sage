const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    defer bw.flush() catch {};

    const filename = "main.sage";
    const max_file_size = 1024 * 1024 * 1024; // 1 GiB
    const contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, max_file_size);
    var tokenizer = Tokenizer.init(contents);

    while (try tokenizer.next()) |token| {
        try bw.writer().print("Token(len={any}): {s}\n", .{ token.contents.len, token.contents });
        try bw.flush();
    }
}

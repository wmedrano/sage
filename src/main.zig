const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Ast = @import("ast.zig").Ast;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    defer bw.flush() catch {};

    const filename = "main.sage";
    const max_file_size = 1024 * 1024 * 1024; // 1 GiB
    const contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, max_file_size);
    var tokenizer = Tokenizer.init(contents);

    try bw.writer().print("Tokens\n", .{});
    while (tokenizer.next()) |token| {
        try bw.writer().print("  Token(type={any}, len={any}): {s}\n", .{ token.typ, token.contents.len, token.contents });
        try bw.flush();
    }

    tokenizer.reset();
    const asts = try Ast.init(&tokenizer, std.heap.page_allocator);
    for (1.., asts) |n, ast| {
        try bw.writer().print("Expression {}\n", .{n});
        try bw.writer().print("{any}", .{ast});
    }
}

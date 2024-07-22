const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const ast = @import("ast.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    defer bw.flush() catch {};

    var timer = try std.time.Timer.start();
    const filename = "main.sage";
    const max_file_size = 1024 * 1024 * 1024; // 1 GiB
    const contents = try std.fs.cwd().readFileAlloc(alloc, filename, max_file_size);
    var tokenizer = Tokenizer.init(contents);
    try bw.writer().print("File read duration: {any}us\n", .{timer.lap() / std.time.ns_per_us});

    try bw.writer().print("Tokens\n", .{});
    while (tokenizer.next()) |token| {
        try bw.writer().print("  Token(type={any}, len={any}): {s}\n", .{ token.typ, token.contents.len, token.contents });
    }
    try bw.writer().print("Tokenizer print duration: {any}us\n", .{timer.lap() / std.time.ns_per_us});

    tokenizer.reset();
    const asts = try ast.Ast.init(&tokenizer, alloc);
    try bw.writer().print("AST build duration: {any}us\n\n", .{timer.lap() / std.time.ns_per_us});
    for (1.., asts) |n, a| {
        try bw.writer().print("Expression {}\n", .{n});
        try bw.writer().print("{any}", .{a});
    }
    try bw.writer().print("\nAST print duration: {any}us\n\n", .{timer.lap() / std.time.ns_per_us});
}

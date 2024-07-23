const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var alloc = gpa.allocator();

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    defer bw.flush() catch {};

    var timer = try std.time.Timer.start();
    const filename = "main.sage";
    const max_file_size = 1024 * 1024 * 1024; // 1 GiB
    const contents = try std.fs.cwd().readFileAlloc(alloc, filename, max_file_size);
    defer alloc.free(contents);
    var tokenizer = Tokenizer.init(contents);
    try bw.writer().print("file-read-duration: {any}us\n\n", .{timer.lap() / std.time.ns_per_us});

    try bw.writer().print("tokens:\n", .{});
    while (tokenizer.next()) |token| {
        try bw.writer().print("  Token(type={any}, len={any}): {s}\n", .{ token.typ, token.contents.len, token.contents });
    }

    tokenizer.reset();
    const asts = try ast.AstCollection.init(&tokenizer, alloc);
    defer asts.deinit();
    for (1.., asts.asts) |n, a| {
        try bw.writer().print("expression: #{}\n", .{n});
        try bw.writer().print("{any}", .{a});
    }
    try bw.writer().print("compile-duration: {any}us\n\n", .{timer.lap() / std.time.ns_per_us});

    var vm = try @import("vm.zig").Vm.init(alloc);
    defer vm.deinit();
    for (1.., asts.asts) |n, a| {
        try bw.writer().print("bytecode: #{}\n", .{n});
        var bc = try bytecode.ByteCodeFunc.init(&a, alloc);
        defer bc.deinit();
        try bw.writer().print("{any}", .{bc});
        const expr_result = try vm.runBytecode(&bc, &.{});
        try bw.writer().print("result: {any}", .{expr_result});
    }
    try bw.writer().print("\nruntime-duration: {any}us\n\n", .{timer.lap() / std.time.ns_per_us});
}

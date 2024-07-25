pub const std = @import("std");
pub const Tokenizer = @import("vm/tokenizer.zig").Tokenizer;
pub const AstCollection = @import("vm/ast.zig").AstCollection;
pub const ByteCodeFunc = @import("vm/bytecode.zig").ByteCodeFunc;
pub const ir = @import("vm/ir.zig");
pub const vm = @import("vm/vm.zig");

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
    const asts = try AstCollection.init(&tokenizer, alloc);
    defer asts.deinit();
    var irs = try std.ArrayList(*ir.Ir).initCapacity(alloc, asts.asts.len);
    defer irs.deinit();
    defer for (irs.items) |i| i.deinit(alloc);
    for (1.., asts.asts) |n, a| {
        try bw.writer().print("\nexpression: #{}\n", .{n});
        try bw.writer().print("{any}", .{a});
        irs.appendAssumeCapacity(try ir.Ir.init(&a, alloc));
    }
    try bw.writer().print("compile-duration: {any}us\n\n", .{timer.lap() / std.time.ns_per_us});

    var v = try vm.Vm.init(alloc);
    defer v.deinit();
    for (1.., irs.items) |n, i| {
        try bw.writer().print("\nbytecode: #{}\n", .{n});
        var bc = try ByteCodeFunc.init(i, alloc);
        defer bc.deinit();
        try bw.writer().print("{any}", .{bc});
        const expr_result = try v.runBytecode(&bc, &.{});
        defer expr_result.deinit(alloc);
        try bw.writer().print("result: {any}\n", .{expr_result});
    }
    try bw.writer().print("\n\nruntime-duration: {any}us\n\n", .{timer.lap() / std.time.ns_per_us});
}

test {
    std.testing.refAllDecls(@This());
}

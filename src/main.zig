const std = @import("std");
const Terminal = @import("term/term.zig").Terminal;
const widgets = @import("term/widgets.zig");
const Event = @import("term/term.zig").Event;

pub fn main() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = allocator.deinit();
    var stats = Stats{};
    try run_program(allocator.allocator(), &stats);
    std.debug.print("{any}\n", .{stats});
}

const Stats = struct {
    exit_reason: []const u8 = "",
    double_buffer_size: usize = 0,

    pub fn format(self: Stats, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print(
            "exit-reason: {s}\ndouble-buffer-size: {d}",
            .{ self.exit_reason, self.double_buffer_size },
        );
    }
};

fn run_program(allocator: std.mem.Allocator, stats: *Stats) !void {
    var term = try Terminal.init(.{ .allocator = allocator });
    defer term.deinit();
    defer stats.double_buffer_size = term.output_buffer.capacity;
    try term.clearDisplay(.screen);
    for (0..std.math.maxInt(usize)) |idx| {
        const event = try term.nextEvent();
        try term.clearDisplay(.screen);
        try term.moveCursor(1, 1);
        try term.writer().print("{d}: {any}\n", .{ idx, event });
        try term.moveCursor(3, 1);
        try term.writer().print("1234567890", .{});
        try widgets.drawRectangle(&term, .{ .row = 4, .col = 1, .width = 40, .height = 10 });
        const key = event.?.key;
        switch (key) {
            .key => |code| switch (code) {
                0x03 => {
                    stats.exit_reason = "Pressed C-c";
                    return;
                },
                else => {
                    try term.moveCursor(2, 1);
                    try term.tty.writer().print("Pressed: {c}\n", .{code});
                },
            },
            .escape => {
                stats.exit_reason = "Pressed ESC";
                return;
            },
            else => {},
        }
        try term.flush();
    }
    stats.exit_reason = "Max number of events handled";
}

test "compile" {
    var stats = Stats{};
    if (false) _ = try run_program(std.testing.allocator, &stats);
}

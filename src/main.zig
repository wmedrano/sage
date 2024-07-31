const std = @import("std");
const Terminal = @import("term.zig").Terminal;
const Event = @import("term.zig").Event;

pub fn main() !void {
    const status = try run_program();
    std.debug.print("{s}\n", .{status});
}

fn run_program() ![]const u8 {
    var term = try Terminal.init(.{});
    defer term.deinit();
    try term.moveCursor(1, 1);
    try term.clearDisplay(.screen);
    for (0..std.math.maxInt(usize)) |idx| {
        const event = try term.nextEvent();
        try term.clearDisplay(.screen);
        try term.moveCursor(1, 1);
        try term.tty.writer().print("{d}: {any}\n", .{ idx, event });
        const key = event.?.key;
        switch (key) {
            .key => |code| switch (code) {
                0x03 => return "C-c",
                else => {
                    try term.moveCursor(2, 1);
                    try term.tty.writer().print("Pressed: {c}\n", .{code});
                },
            },
            .escape => return "Esc",
            else => {},
        }
    }
    return "Max number of events handled";
}

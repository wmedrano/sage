const std = @import("std");
const Terminal = @import("term.zig").Terminal;

pub const Area = struct {
    row: u16,
    col: u16,
    width: u16,
    height: u16,

    pub inline fn bottom(self: Area) u16 {
        return self.row + self.height;
    }

    pub inline fn right(self: Area) u16 {
        return self.col + self.width;
    }
};

pub fn drawRectangle(term: *Terminal, area: Area) !void {
    if (area.width < 2 or area.height < 2) {
        return;
    }

    try term.moveCursor(area.row, area.col);
    _ = try term.writer().write("╭");
    for (2..area.width) |_| {
        _ = try term.writer().write("─");
    }
    _ = try term.writer().write("╮");

    var row = area.row + 1;
    while (row < area.bottom() - 1) {
        try term.moveCursor(row, area.col);
        _ = try term.writer().write("│");
        try term.moveCursor(row, area.right() - 1);
        _ = try term.writer().write("│");
        row += 1;
    }

    try term.moveCursor(area.bottom() - 1, area.col);
    _ = try term.writer().write("╰");
    for (2..area.width) |_| {
        _ = try term.writer().write("─");
    }
    _ = try term.writer().write("╯");
}

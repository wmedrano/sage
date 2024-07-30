pub const std = @import("std");

const Terminal = struct {
    tty: std.fs.File,
    original_terminfo: std.os.linux.termios,

    pub const Options = struct {
        timeout_deciseconds: usize = 1,
    };

    pub fn init(options: Options) !Terminal {
        var tty = try std.fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
        errdefer tty.close();
        if (!tty.isTty()) {
            return error.TerminalNotSupported;
        }
        if (!tty.getOrEnableAnsiEscapeSupport()) {
            return error.TerminalNotSupported;
        }

        var terminfo: std.os.linux.termios = std.mem.zeroes(std.os.linux.termios);
        if (std.os.linux.tcgetattr(tty.handle, &terminfo) != 0) {
            return error.TerminalNotSupported;
        }
        const original_terminfo = terminfo;
        terminfo.lflag.ECHO = false;
        terminfo.lflag.ECHOE = false;
        terminfo.lflag.ECHOK = false;
        terminfo.lflag.ECHONL = false;
        terminfo.lflag.ICANON = false;
        terminfo.lflag.IEXTEN = false;
        terminfo.lflag.ISIG = false;
        terminfo.lflag.NOFLSH = false;
        terminfo.lflag.TOSTOP = false;
        terminfo.cc[std.os.linux.VTIME] = options.timeout_deciseconds;
        if (std.os.linux.tcsetattr(tty.handle, std.os.linux.TCSA.NOW, &terminfo) != 0) {
            return error.TerminalNotSupported;
        }
        errdefer _ = std.os.linux.tcsetattr(tty.handle, std.os.linux.TCSA.NOW, &original_terminfo);

        _ = try tty.write("\x1b[?1049h"); // Enable alternative screen.
        _ = try tty.write("\x1b[?25l"); // Disable cursor.
        return Terminal{
            .tty = tty,
            .original_terminfo = original_terminfo,
        };
    }

    pub fn moveCursor(self: *Terminal, row: u16, col: u16) !void {
        _ = try self.tty.writer().print("\x1b[{d};{d};H", .{ if (row == 0) 1 else row, if (col == 0) 1 else col });
    }

    pub fn clearDisplay(self: *Terminal, position: enum { beforeCursor, afterCursor, screen }) !void {
        const n: u8 = switch (position) {
            .beforeCursor => '0',
            .afterCursor => '1',
            .screen => '2',
        };
        _ = try self.tty.writer().print("\x1b[{c}J", .{n});
    }

    pub fn deinit(self: *Terminal) void {
        _ = self.tty.write("\x1b[?25h") catch {}; // Enable cursor.
        _ = self.tty.write("\x1b[?1049l") catch {}; // Disable alternative screen.
        _ = std.os.linux.tcsetattr(self.tty.handle, std.os.linux.TCSA.NOW, &self.original_terminfo);
        self.tty.close();
    }
};

pub fn main() !void {
    var term = try Terminal.init(.{});
    defer term.deinit();
    try term.moveCursor(1, 1);
    try term.clearDisplay(.screen);
    var buffer: [100]u8 = undefined;
    for (0..20) |idx| {
        std.time.sleep(1 * std.time.ns_per_s);
        const input_size = try term.tty.read(&buffer);
        const input = buffer[0..input_size];
        try term.tty.writer().print("{any}(size={d}): {s}\n", .{ idx, input.len, input });
    }
}

test "terminal" {
    var term = try Terminal.init(.{});
    defer term.deinit();
}

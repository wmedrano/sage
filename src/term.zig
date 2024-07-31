const std = @import("std");

pub const Terminal = struct {
    tty: std.fs.File,
    original_terminfo: std.os.linux.termios,
    input_buffer: [8]u8,

    pub const Options = struct {
        tty: []const u8 = "/dev/tty",
    };

    /// Initialize a new terminal.
    pub fn init(options: Options) !Terminal {
        var tty = try std.fs.cwd().openFile(options.tty, .{ .mode = .read_write });
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
        terminfo.iflag.BRKINT = false;
        terminfo.iflag.ICRNL = false;
        terminfo.iflag.IGNBRK = false;
        terminfo.iflag.IGNCR = false;
        terminfo.iflag.INLCR = false;
        terminfo.iflag.INPCK = false;
        terminfo.iflag.ISTRIP = false;
        terminfo.iflag.IXON = false;
        terminfo.iflag.PARMRK = false;
        terminfo.lflag.ECHO = false;
        terminfo.lflag.ECHOE = false;
        terminfo.lflag.ECHOK = false;
        terminfo.lflag.ECHONL = false;
        terminfo.lflag.ICANON = false;
        terminfo.lflag.IEXTEN = false;
        terminfo.lflag.ISIG = false;
        terminfo.lflag.NOFLSH = false;
        terminfo.lflag.TOSTOP = false;
        terminfo.oflag.OPOST = false;
        terminfo.cflag.CSIZE = std.os.linux.CSIZE.CS8;
        terminfo.cflag.PARENB = false;
        if (std.os.linux.tcsetattr(tty.handle, std.c.TCSA.NOW, &terminfo) != 0) {
            return error.TerminalNotSupported;
        }
        errdefer _ = std.os.linux.tcsetattr(tty.handle, std.c.TCSA.NOW, &original_terminfo);

        _ = try tty.write("\x1b[?1049h"); // Enable alternative screen.
        _ = try tty.write("\x1b[?25l"); // Disable cursor.
        return Terminal{
            .input_buffer = undefined,
            .tty = tty,
            .original_terminfo = original_terminfo,
        };
    }

    /// Move the cursor to the given row and column. row and col are 1 based.
    pub fn moveCursor(self: *Terminal, row: u16, col: u16) !void {
        var writer = self.tty.writer();
        const normalized_row = if (row == 0) 1 else row;
        const normalized_col = if (col == 0) 1 else col;
        _ = try writer.print("\x1b[{d};{d};H", .{ normalized_row, normalized_col });
    }

    /// Clear the display.
    pub fn clearDisplay(self: *Terminal, position: enum { beforeCursor, afterCursor, screen }) !void {
        const n: u8 = switch (position) {
            .beforeCursor => '0',
            .afterCursor => '1',
            .screen => '2',
        };
        _ = try self.tty.writer().print("\x1b[{c}J", .{n});
    }

    // Get the next event.
    pub fn nextEvent(self: *Terminal) !?Event {
        const buff_len = try self.tty.read(&self.input_buffer);
        if (buff_len >= self.input_buffer.len) {
            return error.TerminalInputTooLarge;
        }
        const buff = self.input_buffer[0..buff_len];
        return Event.init(buff);
    }

    /// Restore the terminal to its original state.
    pub fn deinit(self: *Terminal) void {
        _ = self.tty.write("\x1b[?25h") catch {}; // Enable cursor.
        _ = self.tty.write("\x1b[?1049l") catch {}; // Disable alternative screen.
        _ = std.os.linux.tcsetattr(self.tty.handle, std.c.TCSA.NOW, &self.original_terminfo);
        self.tty.close();
    }
};

pub const Event = struct {
    pub const Key = union(enum) {
        key: u8,
        escape,
        backspace,
        raw: []const u8,
    };

    alt: bool = false,
    key: Key,

    pub fn init(data: []const u8) ?Event {
        switch (data.len) {
            0 => return null,
            1 => return switch (data[0]) {
                0x1b => return .{ .key = .escape },
                0x7f => return .{ .key = .backspace },
                else => return .{ .key = .{ .key = data[0] } },
            },
            else => return .{
                .key = .{ .raw = data },
            },
        }
    }
};

test "terminal is supported" {
    var term = try Terminal.init(.{ .tty = "/dev/tty" });
    defer term.deinit();
    // Just testing for compilation.
    if (false) {
        _ = try term.nextEvent();
    }
}

test "terminal is not supported" {
    try std.testing.expectError(error.TerminalNotSupported, Terminal.init(.{ .tty = "/dev/null" }));
}

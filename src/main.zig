const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    defer bw.flush() catch {};
    const script = try Script.init("main.sage", std.heap.page_allocator);
    try script.print(bw.writer());
}

const Script = struct {
    filepath: []u8,
    contents: []u8,
    allocator: std.mem.Allocator,

    pub fn init(filepath: []const u8, allocator: std.mem.Allocator) !Script {
        const input_file = try std.fs.cwd().openFile(filepath, .{});
        const file_size = try input_file.getEndPos();
        const contents = try allocator.alloc(u8, file_size);
        const read_size = try input_file.readAll(contents);
        if (read_size != file_size) {
            return error.UnexpectedNumberOfBytesRead;
        }

        const filepath_copy = try allocator.alloc(u8, filepath.len);
        std.mem.copyForwards(u8, filepath_copy, filepath);

        return .{
            .filepath = filepath_copy,
            .contents = contents,
            .allocator = allocator,
        };
    }

    pub fn free(self: *Script) void {
        self.allocator.free(self.filepath);
        self.allocator.free(self.contents);
    }

    pub fn print(self: *const Script, writer: anytype) !void {
        _ = try writer.print("Script: {s}\n", .{self.filepath});
        _ = try writer.print("{s}", .{self.contents});
    }
};

fn print_script_contents(script_filepath: []const u8, writer: anytype, comptime allocator: std.mem.Allocator) !void {
    var script = try Script.init(script_filepath, allocator);
    defer script.free();
    try script.print(writer);
}

test "print_script_contents reads file" {
    var result = std.ArrayList(u8).init(std.testing.allocator);
    defer result.deinit();
    try print_script_contents("main.sage", result.writer(), std.testing.allocator);
    try std.testing.expectEqualStrings("Script: main.sage\n(+ 1 2 3 4)", result.items);
}

test "print_script_contents with non-existant file returns an error" {
    const writer = struct {
        pub fn print(_: anytype, _: anytype) !usize {
            unreachable;
        }
    };
    try std.testing.expectError(error.FileNotFound, print_script_contents("does-not-exist", writer, std.testing.allocator));
}

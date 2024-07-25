const std = @import("std");

pub const FunctionError = error{ RuntimeError, NotImplemented };

pub const Val = union(Type) {
    pub const Type = enum {
        void,
        symbol,
        boolean,
        int,
        float,
        string,
        function,
    };

    /// Holds a function that may be called with a slice of Val to return a new Val.
    pub const Function = struct {
        /// The name of the function.
        name: []const u8,
        /// The implementation of the function.
        function: *const fn ([]Val) FunctionError!Val,
    };

    pub const String = struct {
        data: []const u8,

        pub fn init(alloc: std.mem.Allocator, data: []const u8) !*String {
            var s = try alloc.create(String);
            s.data = try alloc.dupe(u8, data);
            return s;
        }

        pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
            allocator.free(self.data);
            allocator.destroy(self);
        }
        pub fn format(self: *const String, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{s}", .{self.data});
        }
    };

    /// A none value.
    void,
    /// Contains an immutable symbol. The memory allocation for the slice is not managed by Val.
    symbol: *String,
    /// A bool.
    boolean: bool,
    /// An integer.
    int: i64,
    /// A float.
    float: f64,
    /// A mutable string.
    string: *String,
    /// A function. The memory allocation for this is not managed by Val.
    function: *const Function,

    /// Pretty print the value.
    pub fn format(self: *const Val, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            .void => try writer.print("void", .{}),
            .symbol => |s| try writer.print("symbol({s})", .{s}),
            .boolean => |b| try writer.print("bool({any})", .{b}),
            .int => |n| try writer.print("int({d})", .{n}),
            .float => |n| try writer.print("float({d})", .{n}),
            .string => |s| try writer.print("string({any})", .{s.*}),
            .function => |f| try writer.print("function({s})", .{f.name}),
        }
    }

    /// Returns true if the val is considered truthy. All values except for void and false are
    /// considered truthy.
    pub fn isTruthy(self: *const Val) bool {
        switch (self.*) {
            .void => return false,
            .boolean => |b| return b,
            else => return true,
        }
    }
};

test "val size is small" {
    // TODO: Reduce this to 2 words.
    try std.testing.expectEqual(2 * @sizeOf(usize), @sizeOf(Val));
}

test "val can print" {
    var res = std.ArrayList(u8).init(std.testing.allocator);
    defer res.deinit();
    try res.writer().print("{any}", .{Val{ .int = 0 }});
    try std.testing.expectEqualStrings("int(0)", res.items);
}

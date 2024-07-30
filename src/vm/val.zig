const std = @import("std");
const ByteCodeFunc = @import("bytecode.zig").ByteCodeFunc;
const Vm = @import("vm.zig").Vm;

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
    /// A function.
    function: *const Function,

    /// Holds a function that may be called with a slice of Val to return a new Val.
    pub const Function = struct {
        pub const Error = error{ OutOfMemory, RuntimeError, NotImplemented };

        /// The name of the function.
        name: []const u8,
        /// True if the function is statically allocated. Statically allocated functions do not need
        /// to be freed as they live for the entire program.
        is_static: bool,
        /// The implementation of the function.
        function: union(enum) {
            native: *const fn (*Vm, []Val) Error!Val,
            bytecode: ByteCodeFunc,
        },

        pub fn deinit(self: *Function, allocator: std.mem.Allocator) void {
            if (self.is_static) return;
            allocator.free(self.name);
            switch (self.function) {
                .native => {},
                .bytecode => |*bc| {
                    bc.deinit(allocator);
                },
            }
        }
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
    try std.testing.expectEqual(2 * @sizeOf(usize), @sizeOf(Val));
}

test "val can print" {
    var res = std.ArrayList(u8).init(std.testing.allocator);
    defer res.deinit();
    try res.writer().print("{any}", .{Val{ .int = 0 }});
    try std.testing.expectEqualStrings("int(0)", res.items);
}

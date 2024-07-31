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
        custom,
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
    /// A custom type.
    custom: *const Custom,

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

    /// Contains a custom type that may hold anything.
    pub const Custom = struct {
        /// The name of the type. The pointer should be unique for each type.
        type_name: []const u8,
        /// The pointer to the underlying data.
        ///
        /// TODO: Support lifecycle management.
        data: *void,
    };

    /// Contains a string on the object_manager.
    pub const String = struct {
        len: usize,

        /// Create a new String2 that copies the contents of data.
        pub fn init(allocator: std.mem.Allocator, data: []const u8) !*String {
            const allocation = try allocator.alloc(u8, @sizeOf(usize) + data.len);
            const self = @as(*String, @ptrCast(@alignCast(&allocation[0])));
            self.len = data.len;
            @memcpy(allocation[@sizeOf(usize)..], data);
            return self;
        }

        /// Deallocate the memory from String2.
        pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
            allocator.free(self.rawAllocation());
        }

        /// Return the data held by the String2.
        pub fn asSlice(self: *const String) []u8 {
            return self.rawAllocation()[@sizeOf(usize)..];
        }

        pub fn format(self: *const String, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{s}", .{self.asSlice()});
        }

        /// Get the underlying allocation as a slice. This includes any metadata and data associated
        /// with String2.
        fn rawAllocation(self: *const String) []u8 {
            var result: []u8 = undefined;
            result.len = @sizeOf(usize) + self.len;
            result.ptr = @constCast(@ptrCast(self));
            return result;
        }

        test "init copies slice" {
            const test_string = "test string";
            var s = try String.init(std.testing.allocator, test_string);
            defer s.deinit(std.testing.allocator);
            try std.testing.expectEqualStrings(test_string, s.asSlice());
            try std.testing.expect(test_string.ptr != s.asSlice().ptr);
        }

        test "init handles empty slice" {
            var s = try String.init(std.testing.allocator, "");
            defer s.deinit(std.testing.allocator);
            try std.testing.expectEqualStrings("", s.asSlice());
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
            .custom => |t| try writer.print("custom({s})", .{t.type_name}),
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

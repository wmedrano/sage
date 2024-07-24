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

    /// A none value.
    void,
    /// Contains an immutable symbol. The memory allocation for the slice is not managed by Val.
    symbol: []const u8,
    /// A bool.
    boolean: bool,
    /// An integer.
    int: i64,
    /// A float.
    float: f64,
    /// A mutable string.
    string: []u8,
    /// A function. The memory allocation for this is not managed by Val.
    function: *const Function,

    /// Create a new Val{.string = ...} that holds a copy of string s.
    pub fn initStr(s: []const u8, alloc: std.mem.Allocator) !Val {
        const s_copy = try alloc.alloc(u8, s.len);
        std.mem.copyForwards(u8, s_copy, s);
        return .{ .string = s_copy };
    }

    /// Deallocate any memory associated with Val.
    pub fn deinit(self: Val, alloc: std.mem.Allocator) void {
        switch (self) {
            .void => {},
            .symbol => {},
            .boolean => {},
            .int => {},
            .float => {},
            .string => alloc.free(self.string),
            .function => {},
        }
    }

    /// Clone the Val. alloc is used to deep copy some elements.
    pub fn clone(self: *const Val, alloc: std.mem.Allocator) !Val {
        switch (self.*) {
            .string => |s| {
                const new_s = try alloc.alloc(u8, s.len);
                std.mem.copyForwards(u8, new_s, s);
                return .{ .string = new_s };
            },
            else => return self.*,
        }
    }

    /// Pretty print the value.
    pub fn format(self: *const Val, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            .void => try writer.print("void", .{}),
            .symbol => |s| try writer.print("symbol({s})", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .int => |n| try writer.print("int({d})", .{n}),
            .float => |n| try writer.print("float({d})", .{n}),
            .string => |s| try writer.print("string({s})", .{s}),
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

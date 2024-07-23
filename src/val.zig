const std = @import("std");

pub const ValType = enum {
    symbol,
    int,
    float,
    string,
    function,
};

pub const FunctionError = error{ RuntimeError, NotImplemented };

pub const Function = struct {
    name: []const u8,
    function: *const fn ([]Val) FunctionError!Val,
};

pub const Val = union(ValType) {
    // Contains a symbol. The memory allocation for the slice is not managed by Val.
    symbol: []const u8,
    // An integer.
    int: i64,
    // A float.
    float: f64,
    // A mutable string.
    string: []u8,
    // A function. The memory allocation for this is not managed by Val.
    function: *const Function,

    pub fn init_string(s: []const u8, alloc: std.mem.Allocator) !Val {
        const s_copy = try alloc.alloc(u8, s.len);
        std.mem.copyForwards(u8, s_copy, s);
        return .{ .string = s_copy };
    }

    pub fn deinit(self: Val, alloc: std.mem.Allocator) void {
        switch (self) {
            ValType.symbol => {},
            ValType.int => {},
            ValType.float => {},
            ValType.string => alloc.free(self.string),
            ValType.function => {},
        }
    }

    pub fn clone(self: *const Val, alloc: std.mem.Allocator) !Val {
        switch (self.*) {
            ValType.string => |s| {
                const new_s = try alloc.alloc(u8, s.len);
                std.mem.copyForwards(u8, new_s, s);
                return .{ .string = new_s };
            },
            else => return self.*,
        }
    }

    pub fn format(self: *const Val, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            ValType.symbol => |s| try writer.print("symbol({s})", .{s}),
            ValType.int => |n| try writer.print("int({d})", .{n}),
            ValType.float => |n| try writer.print("float({d})", .{n}),
            ValType.string => |s| try writer.print("string({s})", .{s}),
            ValType.function => |f| try writer.print("function({s})", .{f.name}),
        }
    }
};

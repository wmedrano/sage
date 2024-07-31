const std = @import("std");
const Val = @import("val.zig").Val;

pub const ObjectManager = struct {
    allocator: std.mem.Allocator,
    strings: std.ArrayListUnmanaged(*Val.String),
    customs: std.ArrayListUnmanaged(*Val.Custom),
    global_strings: std.StringHashMapUnmanaged(*Val.String),
    global_functions: std.ArrayListUnmanaged(*Val.Function),

    pub fn init(allocator: std.mem.Allocator) ObjectManager {
        return .{
            .allocator = allocator,
            .strings = std.ArrayListUnmanaged(*Val.String){},
            .customs = std.ArrayListUnmanaged(*Val.Custom){},
            .global_strings = std.StringHashMapUnmanaged(*Val.String){},
            .global_functions = std.ArrayListUnmanaged(*Val.Function){},
        };
    }

    pub fn deinit(self: *ObjectManager) void {
        for (self.strings.items) |s| s.deinit(self.allocator);
        self.strings.deinit(self.allocator);

        var global_strings_iter = self.global_strings.valueIterator();
        while (global_strings_iter.next()) |s| s.*.deinit(self.allocator);
        self.global_strings.deinit(self.allocator);

        for (self.global_functions.items) |f| {
            std.debug.assert(!f.is_static);
            f.deinit(self.allocator);
            self.allocator.destroy(f);
        }
        self.global_functions.deinit(self.allocator);
        for (self.customs.items) |c| self.allocator.destroy(c);
        self.customs.deinit(self.allocator);
    }

    pub fn objectCount(self: *const ObjectManager) usize {
        return self.strings.items.len + self.customs.items.len;
    }

    pub fn removeGarbage(self: *ObjectManager, marked: *const ReferenceMarker) void {
        var dst: usize = 0;
        for (self.strings.items) |string| {
            if (marked.containsString(string)) {
                self.strings.items[dst] = string;
                dst += 1;
            } else {
                string.deinit(self.allocator);
            }
        }
        self.strings.shrinkRetainingCapacity(dst);
    }

    pub fn allocString(self: *ObjectManager, s: []const u8) !Val {
        const s_copy = try Val.String.init(self.allocator, s);
        try self.strings.append(self.allocator, s_copy);
        return Val{ .string = s_copy };
    }

    pub fn allocGlobalString(self: *ObjectManager, s: []const u8) !Val {
        if (self.global_strings.get(s)) |global_s| {
            return .{ .string = global_s };
        }
        const s_copy = try Val.String.init(self.allocator, s);
        try self.global_strings.put(self.allocator, s_copy.asSlice(), s_copy);
        return Val{ .string = s_copy };
    }

    pub fn allocGlobalSymbol(self: *ObjectManager, s: []const u8) !Val {
        if (self.global_strings.get(s)) |global_s| {
            return .{ .symbol = global_s };
        }
        const s_copy = try Val.String.init(self.allocator, s);
        try self.global_strings.put(self.allocator, s_copy.asSlice(), s_copy);
        return Val{ .symbol = s_copy };
    }

    pub fn allocCustom(self: *ObjectManager, CustomType: type, type_name: []const u8, value: *CustomType) !*Val.Custom {
        const res = try self.allocator.create(Val.Custom);
        errdefer self.allocator.destroy(res);
        res.* = Val.Custom{
            .type_name = type_name,
            .data = @ptrCast(value),
        };
        try self.customs.append(self.allocator, res);
        return res;
    }

    pub fn allocFunction(self: *ObjectManager) !*Val.Function {
        const res = try self.allocator.create(Val.Function);
        errdefer self.allocator.destroy(res);
        try self.global_functions.append(self.allocator, res);
        return res;
    }

    /// Pretty print the value.
    pub fn format(self: *const ObjectManager, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("ObjectManager:\n", .{});
        var global_strings_iter = self.global_strings.keyIterator();
        while (global_strings_iter.next()) |s| {
            try writer.print("  global_string: {s}\n", .{s.*});
        }
        for (self.strings.items) |s| {
            try writer.print("  string: {s}\n", .{s});
        }
    }
};

pub const ReferenceMarker = struct {
    allocator: std.mem.Allocator,
    strings: std.AutoHashMapUnmanaged(*Val.String, void),
    customs: std.AutoHashMapUnmanaged(*const Val.Custom, void),

    pub fn init(allocator: std.mem.Allocator) ReferenceMarker {
        return .{
            .allocator = allocator,
            .strings = std.AutoHashMapUnmanaged(*Val.String, void){},
            .customs = std.AutoHashMapUnmanaged(*const Val.Custom, void){},
        };
    }

    pub fn deinit(self: *ReferenceMarker) void {
        self.strings.deinit(self.allocator);
        self.customs.deinit(self.allocator);
    }

    pub fn containsString(self: *const ReferenceMarker, string: *Val.String) bool {
        return self.strings.contains(string);
    }

    pub fn markVals(self: *ReferenceMarker, vals: []const Val) !void {
        for (vals) |v| try self.markVal(v);
    }

    pub fn markVal(self: *ReferenceMarker, val: Val) !void {
        switch (val) {
            .void => {},
            .symbol => |s| try self.markString(s),
            .boolean => {},
            .int => {},
            .float => {},
            .string => |s| try self.markString(s),
            .function => {},
            .custom => |c| try self.markCustom(c),
        }
    }

    pub fn markString(self: *ReferenceMarker, string: *Val.String) !void {
        try self.strings.put(self.allocator, string, {});
    }

    pub fn markCustom(self: *ReferenceMarker, custom: *const Val.Custom) !void {
        try self.customs.put(self.allocator, custom, {});
    }
};

test "object_manager can allocate string" {
    var om = ObjectManager.init(std.testing.allocator);
    defer om.deinit();
    var test_str_val = try Val.String.init(std.testing.allocator, "test-string");
    defer test_str_val.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(
        Val{ .string = test_str_val },
        try om.allocString("test-string"),
    );
}

test "object_manager can allocate custom" {
    var om = ObjectManager.init(std.testing.allocator);
    defer om.deinit();
    const test_type = "test_type";
    var test_data: usize = 100;
    const alloc = try om.allocCustom(usize, test_type, &test_data);
    try std.testing.expectEqual(Val.Custom{
        .type_name = test_type,
        .data = @ptrCast(&test_data),
    }, alloc.*);
}

test "garbage collection removes unused strings" {
    // 2 strings and a global string are allocated
    //   -> object count is 2
    var om = ObjectManager.init(std.testing.allocator);
    defer om.deinit();
    try std.testing.expectEqual(0, om.objectCount());
    _ = try om.allocString("gc-string");
    const keep = try om.allocString("keep-string");
    _ = try om.allocGlobalString("global-string-is-not-counted");
    try std.testing.expectEqual(2, om.objectCount());

    // gc cleans up 1 string and leaves the global string
    //    -> object count is 0
    var refs = ReferenceMarker.init(std.testing.allocator);
    defer refs.deinit();
    try refs.markVal(keep);
    om.removeGarbage(&refs);
    try std.testing.expectEqual(1, om.objectCount());
}

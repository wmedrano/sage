const std = @import("std");
const Val = @import("val.zig").Val;

pub const Heap = struct {
    allocator: std.mem.Allocator,
    strings: std.ArrayListUnmanaged(*Val.String),
    global_strings: std.StringHashMapUnmanaged(*Val.String),
    global_functions: std.ArrayListUnmanaged(*Val.Function),

    pub fn init(allocator: std.mem.Allocator) Heap {
        return .{
            .allocator = allocator,
            .strings = std.ArrayListUnmanaged(*Val.String){},
            .global_strings = std.StringHashMapUnmanaged(*Val.String){},
            .global_functions = std.ArrayListUnmanaged(*Val.Function){},
        };
    }

    pub fn deinit(self: *Heap) void {
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
    }

    pub fn stringCount(self: *const Heap) usize {
        return self.strings.items.len + @as(usize, self.global_strings.size);
    }

    pub fn removeGarbage(self: *Heap, marked: *const ReferenceMarker) void {
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

    pub fn allocString(self: *Heap, s: []const u8) !Val {
        const s_copy = try Val.String.init(self.allocator, s);
        try self.strings.append(self.allocator, s_copy);
        return Val{ .string = s_copy };
    }

    pub fn allocGlobalString(self: *Heap, s: []const u8) !Val {
        if (self.global_strings.get(s)) |global_s| {
            return .{ .string = global_s };
        }
        const s_copy = try Val.String.init(self.allocator, s);
        try self.global_strings.put(self.allocator, s_copy.data, s_copy);
        return Val{ .string = s_copy };
    }

    pub fn allocGlobalSymbol(self: *Heap, s: []const u8) !Val {
        if (self.global_strings.get(s)) |global_s| {
            return .{ .symbol = global_s };
        }
        const s_copy = try Val.String.init(self.allocator, s);
        try self.global_strings.put(self.allocator, s_copy.data, s_copy);
        return Val{ .symbol = s_copy };
    }

    pub fn allocFunction(self: *Heap) !*Val.Function {
        const res = try self.allocator.create(Val.Function);
        try self.global_functions.append(self.allocator, res);
        return res;
    }

    /// Pretty print the value.
    pub fn format(self: *const Heap, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Heap:\n", .{});
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

    pub fn init(allocator: std.mem.Allocator) ReferenceMarker {
        return .{
            .allocator = allocator,
            .strings = std.AutoHashMapUnmanaged(*Val.String, void){},
        };
    }

    pub fn deinit(self: *ReferenceMarker) void {
        self.strings.deinit(self.allocator);
    }

    pub fn reset(self: *ReferenceMarker) void {
        self.strings.clearRetainingCapacity();
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
        }
    }

    pub fn markString(self: *ReferenceMarker, string: *Val.String) !void {
        try self.strings.put(self.allocator, string, {});
    }
};

test "heap can allocate string" {
    var h = Heap.init(std.testing.allocator);
    defer h.deinit();
    var test_str_val = try Val.String.init(std.testing.allocator, "test-string");
    defer test_str_val.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(
        Val{ .string = test_str_val },
        try h.allocString("test-string"),
    );
}

test "garbage collection removes unused strings" {
    var h = Heap.init(std.testing.allocator);
    defer h.deinit();
    try std.testing.expectEqual(0, h.stringCount());
    _ = try h.allocString("gc-string");
    const keep = try h.allocString("keep-string");
    _ = try h.allocGlobalString("global-string-is-kept");
    try std.testing.expectEqual(3, h.stringCount());

    var refs = ReferenceMarker.init(std.testing.allocator);
    defer refs.deinit();
    try refs.markVal(keep);
    h.removeGarbage(&refs);
    try std.testing.expectEqual(2, h.stringCount());
}

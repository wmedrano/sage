pub const std = @import("std");
pub const Vm = @import("vm/vm.zig").Vm;
pub const Val = @import("vm/val.zig").Val;
pub const runBenchmark = @import("tools/benchmark.zig").runBenchmark;

const VmInitBenchmark = struct {
    pub const name: []const u8 = "vm-init";
    alloc: std.mem.Allocator,
    vm: ?Vm = null,

    pub fn run(self: *@This()) !void {
        self.vm = try Vm.init(self.alloc);
    }
    pub fn cleanup(self: *@This()) !void {
        if (self.vm) |*v| v.deinit();
        self.vm = null;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var b = VmInitBenchmark{ .alloc = alloc };
    try runBenchmark(&b, .{ .alloc = alloc });
}

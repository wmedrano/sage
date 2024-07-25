pub const std = @import("std");
pub const Vm = @import("vm/vm.zig").Vm;
pub const Val = @import("vm/val.zig").Val;
pub const ByteCodeFunc = @import("vm/bytecode.zig").ByteCodeFunc;
pub const runBenchmark = @import("tools/benchmark.zig").runBenchmark;

const VmInitBenchmark = struct {
    name: []const u8,
    alloc: std.mem.Allocator,
    vm: ?Vm = null,

    pub fn run(self: *VmInitBenchmark) !void {
        self.vm = try Vm.init(self.alloc);
    }
    pub fn cleanup(self: *VmInitBenchmark) !void {
        if (self.vm) |*v| v.deinit();
        self.vm = null;
    }
};

const VmEvalBenchmark = struct {
    name: []const u8,
    bytecode: ByteCodeFunc,
    vm: Vm,

    pub fn new(alloc: std.mem.Allocator, name: []const u8) !VmEvalBenchmark {
        const vm = try Vm.init(alloc);
        const expr = "(+ (string-length \"string\") 1 2 3)";
        const bytecode = try ByteCodeFunc.initStrExpr(expr, alloc);
        return .{
            .name = name,
            .vm = vm,
            .bytecode = bytecode,
        };
    }

    pub fn run(self: *VmEvalBenchmark) !void {
        var ret = try self.vm.runBytecode(&self.bytecode, &[_]Val{});
        ret.deinit(self.vm.allocator());
    }

    pub fn deinit(self: *VmEvalBenchmark) void {
        self.vm.deinit();
        self.bytecode.deinit();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    var bArena = VmInitBenchmark{ .name = "vm-init-arena", .alloc = arena.allocator() };
    try runBenchmark(VmInitBenchmark, &bArena, .{ .alloc = alloc });

    var b = VmInitBenchmark{ .name = "vm-init", .alloc = alloc };
    try runBenchmark(VmInitBenchmark, &b, .{ .alloc = alloc });

    var evalB = try VmEvalBenchmark.new(alloc, "vm-eval");
    defer evalB.deinit();
    try runBenchmark(VmEvalBenchmark, &evalB, .{ .alloc = alloc });

    var evalArenaB = try VmEvalBenchmark.new(arena.allocator(), "vm-eval-arena");
    defer evalArenaB.deinit();
    try runBenchmark(VmEvalBenchmark, &evalArenaB, .{ .alloc = arena.allocator() });
}

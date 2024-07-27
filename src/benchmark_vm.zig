pub const std = @import("std");

pub const Benchmark = @import("tools/benchmark.zig").Benchmark;
pub const ByteCodeFunc = @import("vm/bytecode.zig").ByteCodeFunc;
pub const Heap = @import("vm/heap.zig").Vm;
pub const Val = @import("vm/val.zig").Val;
pub const Vm = @import("vm/vm.zig").Vm;

const VmInitBenchmark = struct {
    name: []const u8,
    alloc: std.mem.Allocator,
    vm: ?Vm = null,

    pub inline fn run(self: *VmInitBenchmark) !void {
        self.vm = try Vm.init(self.alloc);
    }
    pub inline fn afterRun(self: *VmInitBenchmark) !void {
        if (self.vm) |*v| v.deinit();
        self.vm = null;
    }
};

const VmEvalBenchmark = struct {
    name: []const u8,
    bytecode: ByteCodeFunc,
    vm: Vm,

    pub fn init(alloc: std.mem.Allocator, name: []const u8) !VmEvalBenchmark {
        var vm = try Vm.init(alloc);
        const expr = "(- (string-length \"string\") 1 2 3 (if true (/ 1 4)) (if false 0 10))";
        const bytecode = try ByteCodeFunc.initStrExpr(expr, &vm.heap);
        return .{
            .name = name,
            .vm = vm,
            .bytecode = bytecode,
        };
    }

    pub inline fn run(self: *VmEvalBenchmark) !Val {
        return self.vm.runBytecode(&self.bytecode, &[_]Val{});
    }

    pub inline fn deinit(self: *VmEvalBenchmark) void {
        self.bytecode.deinit(self.vm.allocator());
        self.vm.deinit();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var b = VmInitBenchmark{ .name = "vm-init", .alloc = gpa.allocator() };
    var bResults = try Benchmark.run(VmInitBenchmark, &b, .{ .alloc = gpa.allocator() });
    bResults.deinit();

    var evalB = try VmEvalBenchmark.init(gpa.allocator(), "vm-eval");
    defer evalB.deinit();
    var evalBResults = try Benchmark.run(VmEvalBenchmark, &evalB, .{ .alloc = gpa.allocator() });
    evalBResults.deinit();
}

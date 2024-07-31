pub const std = @import("std");

pub const Benchmark = @import("tools/benchmark.zig").Benchmark;
pub const ByteCodeFunc = @import("vm/bytecode.zig").ByteCodeFunc;
pub const ObjectManager = @import("vm/object_manager.zig").Vm;
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
        const fib_src = "(define fib (lambda (n) (if (< n 1) 0 (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))))";
        var fib = try ByteCodeFunc.initStrExpr(fib_src, &vm.object_manager);
        defer fib.deinit(alloc);
        _ = try vm.runBytecode(&fib, &[_]Val{});
        try vm.runGc();

        const bench_src = "(fib 13)";
        const bench = try ByteCodeFunc.initStrExpr(bench_src, &vm.object_manager);
        return .{
            .name = name,
            .vm = vm,
            .bytecode = bench,
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

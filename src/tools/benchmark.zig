pub const std = @import("std");

pub const Options = struct {
    /// The number of iterations to run as a warmup.
    warmup_samples: usize = 100,
    /// The number of samples to collect.
    samples: usize = 100000,
    /// The allocator to user for intermediate results.
    alloc: std.mem.Allocator,
};

/// Run the benchmarks defined by b. See TestBenchmark for an example benchmark. The benchmark type
/// (BenchmarkType) must have the following properties:
/// name - A constant declaration containing the name of the benchmark.
/// init - A method that takes the *BenchmarkType object and returns !void. Runs befor each sample.
/// cleanup - A methed that takes the *BenchmarkType object and returns !void. Runs after each
///     sample.
/// run - A method that takes the *BenchmarkType object and returns !anytype.
pub fn runBenchmark(b: anytype, options: Options) !void {
    const BType = @TypeOf(b.*);
    const name = BType.name;
    var timer = try std.time.Timer.start();
    std.debug.print("{s:-^80}\n", .{name});
    std.debug.print("warmup-samples: {any}\n", .{options.warmup_samples});
    for (0..options.warmup_samples) |_| {
        if (@hasDecl(BType, "init")) {
            try b.init();
        }
        _ = try b.run();
        if (@hasDecl(BType, "cleanup")) {
            try b.cleanup();
        }
    }
    std.debug.print("benchmark-samples: {any}\n", .{options.samples});
    var result_nanos = try std.ArrayList(u64).initCapacity(options.alloc, options.samples);
    defer result_nanos.deinit();
    for (0..options.samples) |_| {
        if (@hasDecl(BType, "init")) {
            try b.init();
        }
        _ = timer.lap();
        _ = try b.run();
        const nanos = timer.lap();
        result_nanos.appendAssumeCapacity(nanos);
        if (@hasDecl(BType, "cleanup")) {
            try b.cleanup();
        }
    }
    const avg_nanos = Duration.initAvgNanos(result_nanos.items);
    std.debug.print("{s}-runtime: {any}\n", .{ name, avg_nanos });
}

const Duration = struct {
    nanos: f64,

    fn initAvgNanos(nanos: []u64) Duration {
        if (nanos.len == 0) return .{ .nanos = 0.0 };
        var sum: u64 = 0;
        for (nanos) |n| sum += n;
        const avg = @as(f64, @floatFromInt(sum)) / @as(f64, @floatFromInt(nanos.len));
        return Duration.initNanos(avg);
    }

    fn initNanos(nanos: f64) Duration {
        return .{ .nanos = nanos };
    }

    pub fn format(self: *const Duration, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.nanos > std.time.ns_per_s) {
            return writer.print("{d:.2}s", .{self.nanos / std.time.ns_per_s});
        }
        if (self.nanos > std.time.ns_per_ms) {
            return writer.print("{d:.2}ms", .{self.nanos / std.time.ns_per_ms});
        }
        if (self.nanos > std.time.ns_per_us) {
            return writer.print("{d:.2}us", .{self.nanos / std.time.ns_per_us});
        }
        return writer.print("{d:.2}ns", .{self.nanos});
    }
};

const TestBenchmark = struct {
    pub const name: []const u8 = "test-benchmark";
    alloc: std.mem.Allocator,
    lst: ?std.ArrayListUnmanaged(usize) = null,

    /// Run before each iteration. May be omitted if there is nothing to do.
    pub fn init(self: *@This()) !void {
        const len = 10000;
        var lst = try std.ArrayListUnmanaged(usize).initCapacity(self.alloc, len);
        for (0..len) |i| lst.appendAssumeCapacity(i);
        self.lst = lst;
    }

    pub fn run(self: *@This()) !usize {
        var sum: usize = 0;
        for (self.lst.?.items) |i| sum += i;
        return sum;
    }

    pub fn cleanup(self: *@This()) !void {
        if (self.lst) |*lst| {
            lst.deinit(self.alloc);
            self.lst = null;
        }
    }
};

test "test benchmark" {
    var b = TestBenchmark{ .alloc = std.testing.allocator };
    try runBenchmark(&b, .{ .warmup_samples = 10, .samples = 10, .alloc = std.testing.allocator });
}

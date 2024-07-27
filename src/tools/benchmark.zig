pub const std = @import("std");

pub const Benchmark = struct {
    name: []const u8,
    timings: std.ArrayList(u64),

    pub const Options = struct {
        /// The number of iterations to run as a warmup.
        warmup_samples: usize = 100,
        /// The number of samples to collect.
        samples: usize = 100000,
        /// If the results should be printed out.
        print_results: bool = true,
        /// The allocator to user for intermediate results.
        alloc: std.mem.Allocator,
    };

    /// Run the benchmarks defined by b. See TestBenchmark for an example benchmark. The benchmark type
    /// (BenchmarkType) must have the following properties:
    /// name - A declaration or variable containing the string name of the benchmark.
    /// init - A method that takes the *BenchmarkType object and returns !void. Runs befor each sample.
    /// cleanup - A methed that takes the *BenchmarkType object and returns !void. Runs after each
    ///     sample.
    /// run - A method that takes the *BenchmarkType object and returns !anytype.
    pub fn run(BType: type, b: *BType, options: Options) !Benchmark {
        const name = try options.alloc.dupe(u8, @field(b, "name"));
        var timer = try std.time.Timer.start();
        if (options.print_results) {
            std.debug.print("{s:-^80}\n", .{name});
            std.debug.print("warmup-samples: {any}\n", .{options.warmup_samples});
        }
        for (0..options.warmup_samples) |_| {
            if (@hasDecl(BType, "beforeRun")) {
                try b.beforeRun();
            }
            _ = try b.run();
            if (@hasDecl(BType, "afterRun")) {
                try b.afterRun();
            }
        }
        if (options.print_results) {
            std.debug.print("benchmark-samples: {any}\n", .{options.samples});
        }
        var result_nanos = try std.ArrayList(u64).initCapacity(options.alloc, options.samples);
        errdefer result_nanos.deinit();
        for (0..options.samples) |_| {
            if (@hasDecl(BType, "beforeRun")) {
                try b.beforeRun();
            }
            _ = timer.lap();
            _ = try b.run();
            const nanos = timer.lap();
            result_nanos.appendAssumeCapacity(nanos);
            if (@hasDecl(BType, "afterRun")) {
                try b.afterRun();
            }
        }
        const avg_nanos = Duration.initAvgNanos(result_nanos.items);
        if (options.print_results) {
            std.debug.print("{s}-runtime: {any}\n", .{ name, avg_nanos });
        }
        return .{ .name = name, .timings = result_nanos };
    }

    pub fn deinit(self: *Benchmark) void {
        self.timings.allocator.free(self.name);
        self.timings.deinit();
    }
};

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
    name: []const u8 = "test-benchmark",
    alloc: std.mem.Allocator,
    lst: ?std.ArrayListUnmanaged(usize) = null,

    pub fn run(self: *TestBenchmark) !usize {
        var sum: usize = 0;
        for (self.lst.?.items) |i| sum += i;
        return sum;
    }

    pub fn beforeRun(self: *TestBenchmark) !void {
        const len = 10000;
        var lst = try std.ArrayListUnmanaged(usize).initCapacity(self.alloc, len);
        for (0..len) |i| lst.appendAssumeCapacity(i);
        self.lst = lst;
    }

    pub fn afterRun(self: *TestBenchmark) !void {
        if (self.lst) |*lst| {
            lst.deinit(self.alloc);
            self.lst = null;
        }
    }
};

test "test benchmark" {
    var b = TestBenchmark{ .alloc = std.testing.allocator };
    const opts = .{ .warmup_samples = 10, .samples = 10, .print_results = false, .alloc = std.testing.allocator };
    var res = try Benchmark.run(TestBenchmark, &b, opts);
    res.deinit();
    try std.testing.expectEqual(10, res.timings.items.len);
}

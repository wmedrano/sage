const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "sage",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    const test_exe = b.addTest(.{
        .name = "vm-tests",
        .root_source_file = b.path("src/vm/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(test_exe);

    const benchmark_exe = b.addExecutable(.{
        .name = "sage-vm-benchmarks",
        .root_source_file = b.path("src/benchmark_vm.zig"),
        .target = target,
        .optimize = .ReleaseSafe,
    });
    b.installArtifact(benchmark_exe);

    // zig build run
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // zig build test
    const test_cmd = b.addRunArtifact(test_exe);
    if (b.args) |args| test_cmd.addArgs(args);
    const test_run_step = b.step("test", "Run tests");
    test_run_step.dependOn(&test_cmd.step);

    // zig build benchmark
    const benchmark_cmd = b.addRunArtifact(benchmark_exe);
    if (b.args) |args| benchmark_cmd.addArgs(args);
    const benchmark_run_step = b.step("benchmark", "Run benchmarks");
    benchmark_run_step.dependOn(&benchmark_cmd.step);
}

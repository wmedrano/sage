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

    const vm_test_exe = b.addTest(.{
        .name = "vm-tests",
        .root_source_file = b.path("src/vm/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    const term_test_exe = b.addTest(.{
        .name = "term-tests",
        .root_source_file = b.path("src/term/term.zig"),
        .target = target,
        .optimize = optimize,
    });
    const vm_test_cmd = b.addRunArtifact(vm_test_exe);
    const term_test_cmd = b.addRunArtifact(term_test_exe);

    const benchmark_exe = b.addExecutable(.{
        .name = "sage-vm-benchmarks",
        .root_source_file = b.path("src/benchmark_vm.zig"),
        .target = target,
        .optimize = .ReleaseSafe,
    });

    // zig build run
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    const build_step = b.step("build", "Build the app");
    build_step.dependOn(&exe.step);

    // zig build test
    const test_run_step = b.step("test", "Run tests");
    test_run_step.dependOn(&vm_test_cmd.step);
    test_run_step.dependOn(&term_test_cmd.step);

    // zig build benchmark
    const benchmark_cmd = b.addRunArtifact(benchmark_exe);
    if (b.args) |args| benchmark_cmd.addArgs(args);
    const benchmark_run_step = b.step("benchmark", "Run benchmarks");
    benchmark_run_step.dependOn(&benchmark_cmd.step);
}

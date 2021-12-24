const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    var peg_step = buildPeg(b);
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("sizr-format", "src/main.zig");
    exe.step.dependOn(peg_step); // TODO: switch to conditionally depend based on language support or even dynamically linked plugins
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.linkLibC(); // to share a heap with c and eventually support tree_sitter
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

// thank you https://zig.news/xq/zig-build-explained-part-1-59lf
pub fn buildPeg(b: *std.build.Builder) *std.build.Step {
    // TODO: the makefile is so simple probably can just use zig cc instead here
    const make_peg_bins = std.build.RunStep.create(b, "run 'make' in thirdparty peg dep");
    make_peg_bins.addArgs(&[_][]const u8{ "/bin/make", "--directory", "thirdparty/peg-0.1.18" });
    b.getInstallStep().dependOn(&make_peg_bins.step);
    b.installBinFile("thirdparty/peg-0.1.18/peg", "peg");
    //const clean_peg_bins = std.build.RunStep.create(b, "run 'make clean' in thirdparty peg dep");
    //clean_peg_bins.addArgs(&.{ "/bin/make", "--directory", "thirdparty/peg-0.1.18", "clean" });
    //b.addStep();

    return &make_peg_bins.step;
}

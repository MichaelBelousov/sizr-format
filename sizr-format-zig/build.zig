// thank you https://zig.news/xq/zig-build-explained-part-1-59lf

const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    var peg_step = buildPeg(b);
    var tree_sitter_step = buildTreeSitter(b);
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("sizr-format", "src/main.zig");

    // TODO: switch to conditionally depend based on language support or even dynamically linked plugins
    exe.step.dependOn(peg_step);
    exe.step.dependOn(tree_sitter_step);

    exe.setTarget(target);
    exe.setBuildMode(mode);
    // I'm using some C libraries (e.g. tree_sitter), so share the heap
    // (in reality, tree_sitter might avoid a libc dependency)
    exe.linkLibC();
    exe.addIncludeDir("thirdparty/tree-sitter/lib/include");
    exe.addLibPath("thirdparty/tree-sitter");
    exe.linkSystemLibrary("tree-sitter");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var tests = b.addTest("src/main.zig");
    tests.setBuildMode(mode);
    tests.linkLibC();
    tests.addIncludeDir("thirdparty/tree-sitter/lib/include");
    tests.addLibPath("thirdparty/tree-sitter");
    tests.linkSystemLibrary("tree-sitter");

    const test_step = b.step("test", "run tests");
    test_step.dependOn(&tests.step);
}

pub fn buildPeg(b: *std.build.Builder) *std.build.Step {
    const make_peg_bins = std.build.RunStep.create(b, "run 'make' in thirdparty peg dep");
    make_peg_bins.addArgs(&[_][]const u8{ "/bin/make", "--directory", "thirdparty/peg-0.1.18" });
    b.getInstallStep().dependOn(&make_peg_bins.step);
    b.installBinFile("thirdparty/peg-0.1.18/peg", "peg");
    //const clean_peg_bins = std.build.RunStep.create(b, "run 'make clean' in thirdparty peg dep");
    //clean_peg_bins.addArgs(&.{ "/bin/make", "--directory", "thirdparty/peg-0.1.18", "clean" });
    //b.addStep();

    return &make_peg_bins.step;
}

// TODO: abstract the concept of adding a gnumake invocation step (also check if zig has something for this)
pub fn buildTreeSitter(b: *std.build.Builder) *std.build.Step {
    const make_tree_sitter = std.build.RunStep.create(b, "run 'make' in thirdparty tree_sitter dep");
    make_tree_sitter.addArgs(&[_][]const u8{ "/bin/make", "--directory", "thirdparty/tree-sitter" });
    return &make_tree_sitter.step;
}

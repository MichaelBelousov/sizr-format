// thank you https://zig.news/xq/zig-build-explained-part-1-59lf

const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // TODO: replace with .addSystemCommand
    var tree_sitter_step = buildTreeSitter(b);

    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    var tests = b.addTest("src/main.zig");

    const exe = b.addExecutable("tsquery", "src/tsquery.zig");
    exe.step.dependOn(tree_sitter_step);

    exe.setTarget(target);

    // use `-Dtest-filter=x` to filter on tests
    const maybe_test_filter = b.option([]const u8, "test-filter", "Skip tests that do not match the filter");
    if (maybe_test_filter) |test_filter| { tests.setFilter(test_filter); }

    const build_chibi_bindings_src = b.addSystemCommand(&.{ "chibi-ffi", "./tree-sitter-chibi-ffi.scm" });
    const build_chibi_bindings = b.addSharedLibrary("scheme-bindings", "./tree-sitter-chibi-ffi.c", .unversioned);

    build_chibi_bindings.step.dependOn(&build_chibi_bindings_src.step);
    exe.step.dependOn(&build_chibi_bindings.step);

    // zig build-exe -lc -lc++ -Lthirdparty/tree-sitter -Ithirdparty/tree-sitter/lib/include
    // -ltree-sitter thirdparty/tree-sitter-cpp/src/parser.c thirdparty/tree-sitter-cpp/src/scanner.cc src/code.zig
    for ([_]*std.build.LibExeObjStep{exe, tests}) |artifact| {
        artifact.setBuildMode(mode);
        artifact.linkLibC();
        artifact.linkSystemLibrary("c++");
        // TODO: move thirdparty up to share it more appropriately
        artifact.addIncludePath("../thirdparty/tree-sitter/lib/include");
        artifact.addLibraryPath("../thirdparty/tree-sitter");
        artifact.linkSystemLibrary("tree-sitter");
        artifact.addCSourceFile("../thirdparty/tree-sitter-cpp/src/parser.c", &.{"-std=c99"});
        artifact.addCSourceFile("../thirdparty/tree-sitter-cpp/src/scanner.cc", &.{"-std=c++14"});
        artifact.addPackage(std.build.Pkg{
            .name = "tree-sitter",
            .source = std.build.FileSource.relative("../tree-sitter/tree_sitter.zig"),
            .dependencies = null,
        });
    }

    exe.install();

    const test_step = b.step("test", "run tests");
    test_step.dependOn(&tests.step);

    exe.install();
    const run_tsquery_cmd = exe.run();
    run_tsquery_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_tsquery_cmd.addArgs(args);
    }

    const run_tsquery_step = b.step("query", "Run tsquery");
    run_tsquery_step.dependOn(&run_tsquery_cmd.step);
}

// TODO: abstract the concept of adding a gnumake invocation step (also check if zig has something for this)
pub fn buildTreeSitter(b: *std.build.Builder) *std.build.Step {
    const make_tree_sitter = std.build.RunStep.create(b, "run 'make' in thirdparty tree_sitter dep");
    make_tree_sitter.addArgs(&[_][]const u8{ "/bin/make", "--directory", "../thirdparty/tree-sitter" });
    return &make_tree_sitter.step;
}

pub fn buildChibiSchemeBindings(b: *std.build.Builder) *std.build.Step {
    const make_tree_sitter = std.build.RunStep.create(b, "run 'make' in thirdparty tree_sitter dep");
    make_tree_sitter.addArgs(&[_][]const u8{ "/bin/make", "--directory", "../thirdparty/tree-sitter" });
    return &make_tree_sitter.step;
}


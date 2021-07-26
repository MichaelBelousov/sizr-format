use std::path::PathBuf;

fn main() {
    let dir: PathBuf = ["thirdparty", "tree-sitter-python", "src"].iter().collect();
    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        .file(dir.join("scanner.cc"))
        .cpp_link_stdlib("stdc++")
        .compile("tree-sitter-python");
}

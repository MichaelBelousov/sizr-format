// @ts-check

const treesitterExecContext = {
  /** @param {any[]} args */
  x: 5,
  seq: (...args) => ({ seq: args }),
  choice: (...args) => ({ choice: args }),
  field: (...args) => ({ field: args }),
};

/**
 * @param {string[]} grammarPath
 */
exports.main = function(grammarPaths) {
  for (const grammarPath of grammarPaths) {
    // TODO: run in own context with require("vm") to prevent sharing globalThis
    let grammar;
    const execContext = {
      ...treesitterExecContext,
      grammar: (_grammar) => (grammar = _grammar),
    };
    Object.assign(globalThis, treesitterExecContext);
    for (const key in treesitterExecContext) delete globalThis[key];
  }
}

const usage = `
grammar-bindgen generates AST builders that are required by tree-stitcher
from a tree-sitter grammar

Usage: grammar-bindgen [OPTION]... [grammar-path]...

  -h, --help  display this usage text and exit
`;

if (module === require.main) {
  const minimist = require("minimist");
  const cliArgs = minimist(process.argv.slice(2));
  if ("h" in cliArgs || "help" in cliArgs) {
    console.log(usage);
    process.exit();
  }
  if (cliArgs._.length === 0) {
    console.error("Need at least one grammar to generate bindings for");
    console.log(usage);
    process.exit();
  }
  exports.main(cliArgs._);
}


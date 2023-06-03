// @ts-check

/**
 * @param {any} cond
 * @param {string=} msg
 * @returns {asserts cond}
 */
function assert(cond, msg) {
  if (!cond) throw Error(msg || "assertion failed")
}


/** @type {typeof import("web-tree-sitter")} */
// @ts-ignore
const Parser = window.TreeSitter
assert(Parser, "tree-sitter was not already loaded")

const parserInit = Parser.init({
  locateFile() {
    // NOTE: these url's are no where guaranteed to be stable, but something tells me it
    // won't break for a while, and then I can switch to a cdn
    return "https://tree-sitter.github.io/tree-sitter.js"
  },
})

const options = /** @type {HTMLSelectElement | null} */ (document.querySelector("#lang-select"))
assert(options)

/** @type {{[lang: string]: Promise<any>}} */
const languages = {}

options.addEventListener("change", (e) => {
  const langTag = e.currentTarget.value
  let langParser = languages[langTag]

  if (langParser === undefined) {
    languages[langTag] = Parser.Language
      .load(`https://tree-sitter.github.io/tree-sitter-${langTag}.wasm`)
      .then((lang) => {
        // FIXME: do I need a parser? will find out
        const langParser = new Parser()
        langParser.setLanguage(lang)
        return langParser
      })
  }
})


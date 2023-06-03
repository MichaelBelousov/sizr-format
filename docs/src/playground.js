// @ts-check

/**
 * @param {any} cond
 * @param {string=} msg
 * @returns {asserts cond}
 */
function assert(cond, msg) {
  if (!cond) {
    alert(Error(msg || 'assertion failed'))
  }
}

const defaultProgram = `\
(transform
  ((function_definition body: (_) @body))
  (@body )
  (playground-workspace))
`

const defaultTarget = `\
def myfunc(a, b):
  return a + b
`

let sessionProgram = sessionStorage.getItem('program')
sessionProgram = sessionProgram && sessionProgram.trim() === '' ? defaultProgram : sessionProgram

let sessionTarget = sessionStorage.getItem('target')
const targetInSessionStorageIsValid = sessionTarget && sessionTarget.trim() === ''
sessionTarget = targetInSessionStorageIsValid ? defaultTarget : sessionTarget

let sessionTargetType = sessionStorage.getItem('target-type')
sessionTargetType = targetInSessionStorageIsValid ? 'python' : sessionTargetType

const programEditor = /** @type {HTMLTextAreaElement} */ (document.querySelector('#program-editor'))
programEditor.value = sessionProgram

const targetEditor = /** @type {HTMLTextAreaElement} */ (document.querySelector('#target-editor'))
targetEditor.value = sessionTarget

const langSelect = /** @type {HTMLSelectElement} */ (document.querySelector('#lang-select'))
langSelect.value = sessionTargetType || 'python'

const output = /** @type {HTMLPreElement} */ (document.querySelector('#output'))
const runButton = /** @type {HTMLButtonElement} */ (document.querySelector('#run-btn'))

targetEditor.addEventListener('change', (e) => {
  sessionTarget = e.currentTarget.value
  sessionStorage.setItem('target', sessionTarget)
})

programEditor.addEventListener('change', (e) => {
  sessionProgram = e.currentTarget.value
  sessionStorage.setItem('program', sessionProgram)
})

programEditor.addEventListener('keydown', (e) => {
  if (e.ctrlKey && e.key === "Enter") {
    runButton.click();
  }
})


/** @type {typeof import('web-tree-sitter')} */
// @ts-ignore
const Parser = window.TreeSitter
assert(Parser, 'tree-sitter was not already loaded')

const parserInit = Parser.init({
  locateFile() {
    // NOTE: these url's are no where guaranteed to be stable, but something tells me it
    // won't break for a while, and then I can switch to a cdn
    return 'https://tree-sitter.github.io/tree-sitter.wasm'
  },
})

/** @type {{[lang: string]: Promise<any>}} */
const languages = {}

langSelect.addEventListener('change', (e) => {
  const langTag = e.currentTarget.value
  sessionStorage.setItem('target-type', langTag)
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

// @ts-ignore
const Chibi = window.Chibi
assert(Parser, 'Chibi was not already loaded')


// TODO: disable run when already running
runButton.addEventListener('click', () => {
  Chibi({
    /** @param {string} text */
    print(text) {
      output.value += text + '\n'
    },
    /** @param {string} text */
    printErr(text) {
      output.value += 'ERROR\n:' + text + '\n'
    },
    // HACK
    program: programEditor.value + "(wait-on-event!)",
    // NOTE: can I use this to send the target text?
    arguments: [targetEditor.value],
  })
})


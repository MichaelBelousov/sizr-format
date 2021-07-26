
/** 
maintain a prism.js fork with this
*/
const regex_metachars = '*$^{}[]|?+.\\'

const regex_metachar_pattern = RegExp(Array.from(regex_metachars, c => `\\${c}`).join('|'), 'g')

function escapeRegexSrc(str: string): string {
  return str.replace(regex_metachar_pattern, m => `\\${m}`)
}

function patternFromLiteralText(str: string): RegExp {
  return RegExp(escapeRegexSrc(str))
}

function orPatterns(...regex: RegExp[]): RegExp {
  return RegExp(regex.map(r => r.source).join('|')
}

function catPatterns(...regex: RegExp[]): RegExp {
  return RegExp(regex.map(r => r.source).reduce((prev, cur) => prev + cur))
}


path_pattern = /(\.\/|\/)?([^\/]*\/)*[^\/]*/

regex_literal_pattern = /\/[^\/]*\//

identifier = /[a-zA-Z_][a-zA-Z_0-9]*/

// may want to import or declare types for Prism global
Prism.languages.sizr =
  { comment: /##[^\n\r]*[\n\r]?/
  , positional: orPatterns( ...['@^', '@$', '@^^', '@$$', '@>!', '@<!', '@.'].map(patternFromLiteralText)
                          , ...['@./', '@/'].map(m => catPatterns(patternFromLiteralText(m), path_pattern))
                          , /@>+/
                          )
  , capture:
    { pattern: orPatterns( catPatterns(patternFromLiteralText('$'), identifier)
                         , catPatterns(patternFromLiteralText('$'), regex_literal_pattern)
                         , patternFromLiteralText('$')
                         )
    }
  , transform_operator: orPatterns(...['>>>', '>>!', ';;;'].map(m => patternFromLiteralText(m)))
  , nesting_operator: orPatterns(...['.', '::', '(', '[', '<', '{', ':', ';', ',', '@', '#'].map(m => patternFromLiteralText(m)))
  , property: catPatterns(/!?/, identifier, /(=\S+)?/)
  };

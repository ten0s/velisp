const path = require('path')
const {Sym, Str, List, Pair, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    // VeLisp Extension
    context.setSym('VE-FILENAME-PARSE', new Fun('ve-filename-parse', ['filename'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('ve-filename-parse: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('ve-filename-parse: too many arguments')
        }
        const filename = ensureType('ve-filename-parse:', args[0], [Str]).value()
        // Win32 workaround
        const fixed = fixWin32(filename)
        const parsed = path.win32.parse(fixed)
        const isFixed = filename !== fixed
        return new List([
            new Pair(new Sym('root'), new Str(maybeUnfixWin32(isFixed, parsed['root']))),
            new Pair(new Sym('dir'),  new Str(maybeUnfixWin32(isFixed, parsed['dir']))),
            new Pair(new Sym('base'), new Str(parsed['base'])),
            new Pair(new Sym('name'), new Str(parsed['name'])),
            new Pair(new Sym('ext'),  new Str(parsed['ext'])),
        ])
    }))
}

const fixWin32 = (str) =>
    str.split('\\\\').join('\\')

const unfixWin32 = (str) =>
    str.split('\\').join('\\\\')

const maybeUnfixWin32 = (todo, str) =>
    todo ? unfixWin32(str) : str

import path from 'path'
import {Sym, Str, List, Pair, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    // VeLisp Extension
    context.setSym('FILENAME-PARSE', new Fun('filename-parse', ['filename'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('filename-parse: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('filename-parse: too many arguments')
        }
        const filename = ensureType('filename-parse:', args[0], [Str]).value()
        const parsed = path.win32.parse(filename)
        return new List([
            new Pair(new Sym('root'), new Str(parsed['root'])),
            new Pair(new Sym('dir'),  new Str(parsed['dir'])),
            new Pair(new Sym('base'), new Str(parsed['base'])),
            new Pair(new Sym('name'), new Str(parsed['name'])),
            new Pair(new Sym('ext'),  new Str(parsed['ext'])),
        ])
    }))
}

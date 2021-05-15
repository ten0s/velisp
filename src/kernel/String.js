const VeGlob = require('../VeGlob.js')
const VeRegex = require('../VeRegex.js')
const VeWildcard = require('../VeWildcard.js')
const {inspect} = require('../VeUtil.js')
const {Bool, Int, Real, Str, Sym, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    context.setSym('ASCII', new Fun('ascii', ['string'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('ascii: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('ascii: too many arguments')
        }
        const str = ensureType('ascii:', args[0], [Str]).value()
        if (!str.length) {
            throw new Error('ascii: expected non-empty Str')
        }
        return new Int(str.charCodeAt())
    }))
    context.setSym('CHR', new Fun('chr', ['int'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('chr: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('chr: too many arguments')
        }
        const int = ensureType('chr:', args[0], [Int]).value()
        return new Str(String.fromCharCode(int))
    }))
    context.setSym('ITOA', new Fun('itoa', ['int'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('itoa: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('itoa: too many arguments')
        }
        return new Str(ensureType('itoa:', args[0], [Int]).toString())
    }))
    context.setSym('ATOI', new Fun('atoi', ['str'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('atoi: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('atoi: too many arguments')
        }
        const arg = ensureType('atoi:', args[0], [Str])
        try {
            const val = Number.parseInt(arg.str)
            if (Number.isInteger(val)) {
                return new Int(val)
            }
        } catch (e) {}
        return new Int(0)
    }))
    context.setSym('ATOF', new Fun('atof', ['str'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('atof: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('atof: too many arguments')
        }
        const arg = ensureType('atof:', args[0], [Str])
        try {
            const val = Number.parseFloat(arg.str)
            if (Number.isFinite(val)) {
                return new Real(val)
            }
        } catch (e) {}
        return new Real(0.0)
    }))
    context.setSym('STRCASE', new Fun('strcase', ['str [which]'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('strcase: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('strcase: too many arguments')
        }
        const str = ensureType('strcase:', args[0], [Str])
        if (args.length === 2) {
            if (!args[1].isNil()) {
                return str.toLowerCase()
            }
        }
        return str.toUpperCase()
    }))
    context.setSym('STRCAT', new Fun('strcat', ['[str] ...'], [], (self, args) => {
        let result = new Str('')
        for (const arg of args) {
            result = result.concat(ensureType('strcat:', arg, [Str]))
        }
        return result
    }))
    context.setSym('STRLEN', new Fun('strlen', ['[str] ...'], [], (self, args) => {
        let result = 0
        for (const arg of args) {
            result += ensureType('strlen:', arg, [Str]).length()
        }
        return new Int(result)
    }))
    context.setSym('SUBSTR', new Fun('substr', ['string start [length]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('substr: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('substr: too many arguments')
        }
        const string = ensureType('substr: `string`', args[0], [Str])
        const start = ensureType('substr: `start`', args[1], [Int]).value()
        if (start > 0) {
            if (args.length === 3) {
                const length = ensureType('substr: `length`', args[2], [Int]).value()
                if (length >= 0) {
                    return string.substring(start - 1, length)
                }
                throw new Error('substr: `length` expected non-negative Int')
            }
            return string.substring(start - 1, string.length())
        }
        throw new Error('substr: `start` expected positive Int')
    }))
    // VeLisp Extension 'flag'
    context.setSym('WCMATCH', new Fun('wcmatch', ['str', 'pattern', '[flag]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('wcmatch: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('wcmatch: too many arguments')
        }
        const str = ensureType('wcmatch: `str`', args[0], [Str]).value()
        const pat = ensureType('wcmatch: `pattern`', args[1], [Str]).value()
        const wc = new VeWildcard(pat)
        let flag = new Bool(false)
        if (args.length === 3) {
            flag = ensureType('wcmatch: `flag`', args[2], [Sym, Bool])
        }
        if (flag instanceof Sym) {
            switch (flag.value()) {
            case 'INSPECT':
                console.error(inspect(wc))
                break
            case 'REGEX':
                console.error(inspect(wc.toRegex()))
                break
            case 'DOT':
                console.error(wc.toDot().trimEnd())
                break
            default:
                console.error(`Unknown flag: ${flag.value()}`)
                break
            }
        }
        return new Bool(wc.test(str))
    }))
    // VeLisp Extension
    context.setSym('GLOBMATCH', new Fun('globmatch', ['str', 'pattern', '[flag]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('globmatch: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('globmatch: too many arguments')
        }
        const str = ensureType('globmatch: `str`', args[0], [Str]).value()
        const pat = ensureType('globmatch: `pattern`', args[1], [Str]).value()
        const glob = new VeGlob(pat)
        let flag = new Bool(false)
        if (args.length === 3) {
            flag = ensureType('globmatch: `flag`', args[2], [Sym, Bool])
        }
        if (flag instanceof Sym) {
            switch (flag.value()) {
            case 'INSPECT':
                console.error(inspect(glob))
                break
            case 'REGEX':
                console.error(inspect(glob.toRegex()))
                break
            case 'DOT':
                console.error(glob.toDot().trimEnd())
                break
            default:
                console.error(`Unknown flag: ${flag.value()}`)
                break
            }
        }
        return new Bool(glob.test(str))
    }))
    // VeLisp Extension
    context.setSym('REMATCH', new Fun('rematch', ['str', 'pattern', '[flag]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('rematch: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('rematch: too many arguments')
        }
        const str = ensureType('rematch: `str`', args[0], [Str]).value()
        const pat = ensureType('rematch: `pattern`', args[1], [Str]).value()
        const re = new VeRegex(pat)
        let flag = new Bool(false)
        if (args.length === 3) {
            flag = ensureType('rematch: `flag`', args[2], [Sym, Bool])
        }
        if (flag instanceof Sym) {
            switch (flag.value()) {
            case 'INSPECT':
                console.error(inspect(re))
                break
            case 'DOT':
                console.error(re.toDot().trimEnd())
                break
            default:
                console.error(`Unknown flag: ${flag.value()}`)
                break
            }
        }
        return new Bool(re.test(str))
    }))
}

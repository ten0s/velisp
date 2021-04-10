const {Int, Real, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = function (context) {
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
            if (Number.isFinite(val)) {
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
            const val = parseFloat(arg.str)
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
        const start = ensureType('substr: `start`', args[1], [Int])
        if (start.value() > 0) {
            if (args.length === 3) {
                const length = ensureType('substr: `length`', args[2], [Int])
                if (length.value() >= 0) {
                    return string.substring(start.value() - 1, length.value())
                }
                throw new Error('substr: `length` expected non-negative Int')
            }
            return string.substring(start.value() - 1, string.length())
        }
        throw new Error('substr: `start` expected positive Int')
    }))
}

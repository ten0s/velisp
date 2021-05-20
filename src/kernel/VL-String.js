const {Bool, Int, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    context.setSym('VL-PRIN1-TO-STRING', new Fun('vl-print1-to-string', ['data'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-prin1-to-string: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-prin1-to-string: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof Str) {
            // Return new Str with escaped content
            return new Str(arg.toString())
        }
        return new Str(arg.toString())
    }))
    context.setSym('VL-PRINC-TO-STRING', new Fun('vl-princ-to-string', ['data'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-princ-to-string: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-princ-to-string: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof Str) {
            // Return the same Str
            return arg
        }
        return new Str(arg.toString())
    }))
    context.setSym('VL-STRING-POSITION', new Fun('vl-string-position', ['char-code', 'string', '[start-pos [from-end-p]]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('vl-string-position: too few arguments')
        }
        if (args.length > 4) {
            throw new Error('vl-string-position: too many arguments')
        }
        const code = ensureType('vl-string-position: `char-code`', args[0], [Int]).value()
        const string = ensureType('vl-string-position: `string`', args[1], [Str]).value()
        let startPos = 0
        let fromEnd = false
        if (args.length === 3) {
            const arg = ensureType('vl-string-position: `start-pos`', args[2], [Int, Bool])
            if (arg instanceof Int) {
                startPos = arg.value()
            }
            if (startPos < 0) {
                throw new Error('vl-string-position: `start-pos` expected non-negative Int')
            }
        }
        if (args.length === 4) {
            const arg = ensureType('vl-string-position: `start-pos`', args[2], [Int, Bool])
            if (arg instanceof Int) {
                startPos = arg.value()
            }
            if (startPos < 0) {
                throw new Error('vl-string-position: `start-pos` expected non-negative Int')
            }
            fromEnd = ensureType('vl-string-position: `from-end-p`', args[3], [Bool]).value()
        }
        let suffix = string
        if (startPos > 0) {
            suffix = string.slice(startPos)
        }
        if (!fromEnd) {
            for (let i = 0; i < suffix.length; i++) {
                if (suffix.charCodeAt(i) === code) {
                    return new Int(i + startPos)
                }
            }
        } else {
            for (let i = suffix.length - 1; i >= 0; i--) {
                if (suffix.charCodeAt(i) === code) {
                    return new Int(i + startPos)
                }
            }
        }
        return new Bool(false)
    }))
    context.setSym('VL-STRING-SEARCH', new Fun('vl-string-search', ['pattern', 'string', '[start-pos]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('vl-string-search: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('vl-string-search: too many arguments')
        }
        const pattern = ensureType('vl-string-search: `pattern`', args[0], [Str]).value()
        const string = ensureType('vl-string-search: `string`', args[1], [Str]).value()
        let startPos = 0
        if (args.length === 3) {
            startPos = ensureType('vl-string-search: `start-pos`', args[2], [Int]).value()
            if (startPos < 0) {
                throw new Error('vl-string-search: `start-pos` expected non-negative Int')
            }
        }
        let suffix = string
        if (startPos > 0) {
            suffix = string.slice(startPos)
        }
        const pos = suffix.indexOf(pattern)
        if (pos >= 0) {
            return new Int(pos + startPos)
        }
        return new Bool(false)
    }))
}

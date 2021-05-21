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
    context.setSym('VL-STRING-MISMATCH', new Fun('vl-string-mismatch', ['str1', 'str1', '[pos1]', '[pos2]', '[ignore-case-p]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('vl-string-mismatch: too few arguments')
        }
        if (args.length > 5) {
            throw new Error('vl-string-mismatch: too many arguments')
        }
        let str1 = ensureType('vl-string-mismatch: `str1`', args[0], [Str]).value()
        let str2 = ensureType('vl-string-mismatch: `str2`', args[1], [Str]).value()
        let pos1 = 0
        let pos2 = 0
        let icase = false
        if (args.length > 2) {
            pos1 = ensureType('vl-string-mismatch: `pos1`', args[2], [Int]).value()
            if (pos1 < 0) {
                throw new Error('vl-string-mismatch: `pos1` expected non-negative Int')
            }
        }
        if (args.length > 3) {
            pos2 = ensureType('vl-string-mismatch: `pos2`', args[3], [Int]).value()
            if (pos2 < 0) {
                throw new Error('vl-string-mismatch: `pos2` expected non-negative Int')
            }
        }
        if (args.length > 4) {
            icase = ensureType('vl-string-mismatch: `ignore-case-p`', args[4], [Bool]).value()
        }
        if (icase) {
            str1 = str1.toLowerCase()
            str2 = str2.toLowerCase()
        }
        if (pos1 > 0) {
            str1 = str1.slice(pos1)
        }
        if (pos2 > 0) {
            str2 = str2.slice(pos2)
        }
        const len = Math.min(str1.length, str2.length)
        for (let i = 0; i < len; i++) {
            if (str1.charCodeAt(i) !== str2.charCodeAt(i)) {
                return new Int(i)
            }
        }
        return new Int(len)
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
    context.setSym('VL-STRING-SUBST', new Fun('vl-string-subst', ['new-str', 'pattern', 'str', '[start-pos]'], [], (self, args) => {
        if (args.length < 3) {
            throw new Error('vl-string-subst: too few arguments')
        }
        if (args.length > 4) {
            throw new Error('vl-string-subst: too many arguments')
        }
        const newstr = ensureType('vl-string-subst: `new-str`', args[0], [Str]).value()
        const pattern = ensureType('vl-string-subst: `pattern`', args[1], [Str]).value()
        const string = ensureType('vl-string-subst: `str`', args[2], [Str]).value()
        let startPos = 0
        if (args.length === 4) {
            startPos = ensureType('vl-string-subst: `start-pos`', args[3], [Int]).value()
            if (startPos < 0) {
                throw new Error('vl-string-subst: `start-pos` expected non-negative Int')
            }
        }
        let prefix = ''
        let suffix = string
        if (startPos > 0) {
            prefix = string.slice(0, startPos)
            suffix = string.slice(startPos)
        }
        suffix = suffix.replace(pattern, newstr)
        return new Str(prefix + suffix)
    }))
}

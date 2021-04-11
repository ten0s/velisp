const {Bool, Int, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = function (context) {
    context.setSym('VL-PRIN1-TO-STRING', new Fun('vl-print1-to-string', ['data'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-prin1-to-string: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-prin1-to-string: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof Str) {
            return new Str(arg.toEscapedString())
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
            return arg
        }
        return new Str(arg.toString())
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
        let shifted = string
        if (startPos > 0) {
            shifted = string.slice(startPos)
        }
        const pos = shifted.indexOf(pattern)
        if (pos >= 0) {
            return new Int(pos + startPos)
        }
        return new Bool(false)
    }))
}

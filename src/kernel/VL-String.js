const {Str, Fun} = require('../VeLispTypes.js')

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
}

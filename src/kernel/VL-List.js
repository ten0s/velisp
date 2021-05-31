const {Bool, List, Pair, Fun} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    context.setSym('VL-CONSP', new Fun('vl-consp', ['list'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-consp: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-consp: too many arguments')
        }
        const arg = args[0]
        if (arg.isNil()) {
            return new Bool(false)
        }
        if (arg instanceof List || arg instanceof Pair) {
            return new Bool(true)
        }
        return new Bool(false)
    }))
}

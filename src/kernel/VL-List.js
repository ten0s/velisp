const {Bool, Int, List, Pair, Fun, ensureType} = require('../VeLispTypes.js')

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
    context.setSym('VL-LIST*', new Fun('vl-list*', ['object', '[object ...]'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('vl-list*: too few arguments')
        }
        if (args.length === 1) {
            return args[0]
        }
        let result = undefined
        let last = args[args.length-1]
        let prelast = args[args.length-2]
        if (last.isNil()) {
            result = new List([prelast])
        }
        if (last instanceof List || last instanceof Pair) {
            result = last.cons(prelast)
        } else {
            result = new Pair(prelast, last)
        }
        for (let i = args.length-3; i >= 0; i--) {
            result = result.cons(args[i])
        }
        return result
    }))
    context.setSym('VL-LIST-LENGTH', new Fun('vl-list-length', ['list-or-cons-object'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('vl-list-length: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-list-length: too many arguments')
        }
        const arg = args[0]
        if (arg.isNil()) {
            return new Int(0)
        }
        if (arg instanceof List) {
            if (arg.last() instanceof Pair) {
                return new Bool(false)
            }
            return new Int(arg.length())
        }
        if (arg instanceof Pair) {
            return new Bool(false)
        }
        ensureType('vl-list-length:', arg, [List, Pair])
    }))
}

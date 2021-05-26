const {Bool, Sym, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    context.setSym('BOUNDP', new Fun('boundp', ['sym'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('boundp: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('boundp: too many arguments')
        }
        const name = args[0] // Allow everything
        const value = self.contexts[self.contexts.length-1].getVar(name)
        if (value instanceof Bool && value.value() === false) {
            // sym is undefined, but what's the point to automatically create it?
            return new Bool(false)
        }
        return new Bool(true)
    }))
    context.setSym('SET', new Fun('set', ['sym', 'expr'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('set: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('set: too many arguments')
        }
        const name = ensureType('set: `sym`', args[0], [Sym])
        const value = args[1]
        //console.error(`set: ${name} = ${value}`);
        self.contexts[self.contexts.length-1].setVar(name, value)
        return value
    }))
    context.setSym('TYPE', new Fun('type', ['item'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('type: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('type: too many arguments')
        }
        return args[0].type()
    }))
}

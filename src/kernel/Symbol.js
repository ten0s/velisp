const {Sym, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
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

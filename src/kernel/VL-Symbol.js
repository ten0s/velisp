const {Bool, Str, Sym, Fun, ensureType} = require('../VeLispTypes.js');

exports.initContext = function (context) {
    context.setSym('VL-SYMBOL-NAME', new Fun('vl-symbol-name', ['sym'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('vl-symbol-name: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('vl-symbol-name: too many arguments');
        }
        const arg = ensureType('vl-symbol-name:', args[0], [Sym]);
        return new Str(arg.toString());
    }));
    context.setSym('VL-SYMBOL-VALUE', new Fun('vl-symbol-value', ['sym'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('vl-symbol-value: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('vl-symbol-value: too many arguments');
        }
        const arg = ensureType('vl-symbol-value:', args[0], [Sym]);
        return self.contexts[self.contexts.length-1].getSym(arg.value());
    }));
    context.setSym('VL-SYMBOLP', new Fun('vl-symbolp', ['obj'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('vl-symbolp: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('vl-symbolp: too many arguments');
        }
        return new Bool(args[0] instanceof Sym);
    }));
}

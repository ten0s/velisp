const {Bool, Fun} = require('../VeLispTypes.js');

//
// Kernel Symbol-Handling Functions
//

exports.addTo = function (context) {
    context.setSym('apply', new Fun('apply', ['function', 'list'], function (self, args) {
        // TODO: check args
        //console.log(args);
        const sym = args[0].value();
        const list = args[1].value();
        const fun = self.contexts[self.contexts.length-1].getSym(sym);
        if (!fun.isNil()) {
            //console.log(list);
            return fun.apply(self, list);
        }
        throw new Error(`apply: no such function ${sym}`);
    }));
    context.setSym('not', new Fun('not', ['item'], function (self, args) {
        return new Bool(args[0].isNil());
    }));
    context.setSym('null', context.getSym('not'));
}

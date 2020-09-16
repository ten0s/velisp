const {Bool, Sym, List, Fun} = require('../VeLispTypes.js');

//
// Function-Handling Functions
//

exports.initContext = function (context) {
    context.setSym('APPLY', new Fun('apply', ['function', 'list'], function (self, args) {
        //console.log('apply args', args);
        if (args.length < 2) {
            throw new Error('apply: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('apply: too many arguments');
        }
        let fun = args[0];
        if (fun instanceof Sym) {
            // Try resolving symbol to function
            fun = self.contexts[self.contexts.length-1].getSym(fun.value());
        }
        if (fun instanceof Fun) {
            let list = args[1];
            if (list instanceof List) {
                list = list.value();
            } else if (list.isNil()) {
                list = [];
            } else {
                throw new Error('apply: list must be List');
            }
            return fun.apply(self, list);
        }
        throw new Error(`apply: no such function ${args[0]}`);
    }));
}

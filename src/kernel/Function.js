const {Bool, Fun} = require('../VeLispTypes.js');

//
// Function-Handling Functions
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
}

const fs = require('fs');
const {Bool, Str, Sym, Fun} = require('../VeLispTypes.js');
const {evaluate} = require('../VeLispEvaluator.js');

//
// Application-Handling Functions
//

exports.addTo = function (context) {
    context.setSym('cwd', new Fun('cwd', [], function (self, args) {
        if (args.length > 0) {
            throw new Error('cwd: too many arguments');
        }
        return new Str(process.cwd());
    }));
    context.setSym('load', new Fun('load', ['filename', '[onfailure]'], function (self, args) {
        // TODO: check args
        //console.log(args);
        const filename = args[0].value();
        const data = fs.readFileSync(filename).toString();
        // FunCall pushes new context just before the call
        // and pops it after the call.
        // Since (load filename) can defun other functions
        // we need to store them in the parent context.
        const context = self.contexts[self.contexts.length-2];
        evaluate(data, context);
        return new Sym('ok');
        //throw new Error(`load: error`);
    }));
}

const fs = require('fs');
const path = require('path');

const {Bool, Str, Sym, Fun} = require('../VeLispTypes.js');
const Evaluator = require('../VeLispEvaluator.js');
const {fmtError} = require('../VeLispError.js');

//
// Application-Handling Functions
//

exports.initContext = function (context) {
    context.setSym('LOAD', new Fun('load', ['filename', '[onfailure]'], [], (self, args) => {
        //console.log('load args:', args);
        if (args.length == 0) {
            throw new Error('load: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('load: too many arguments');
        }
        if (!(args[0] instanceof Str)) {
            throw new Error('load: `filename` expected Str');
        }
        const filename = maybeAddExt(args[0].value());
        try {
            const data = fs.readFileSync(filename).toString();
            // FunCall pushes new context just before the call
            // and pops it after the call.
            // Since (load filename) can defun other functions
            // we need to store them in the parent context.
            const context = self.contexts[self.contexts.length-2];
            return Evaluator.evaluate(data, context);
        } catch (e) {
            if (args.length == 2) {
                let onfailure = args[1];
                if (onfailure instanceof Sym) {
                    // Try resolving symbol to function
                    onfailure = self.contexts[self.contexts.length-1].getSym(onfailure.value());
                }
                if (onfailure instanceof Fun) {
                    return onfailure.apply(self, []);
                }
                return args[1];
            }
            e.path = filename;
            throw new Error(fmtError('load', e));
        }
    }));
}

function maybeAddExt(filename) {
    if (path.extname(filename)) {
        return filename;
    }
    return filename + '.lsp';
}

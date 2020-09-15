const fs = require('fs');
const path = require('path');

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
        //console.log('load args:', args);
        if (args.length == 0) {
            throw new Error('load: too few arguments');
        }
        if (!(args[0] instanceof Str)) {
            throw new Error('load: filename must be Str');
        }
        const filename = maybeAddExt(args[0].value());
        try {
            const data = fs.readFileSync(filename).toString();
            // FunCall pushes new context just before the call
            // and pops it after the call.
            // Since (load filename) can defun other functions
            // we need to store them in the parent context.
            const context = self.contexts[self.contexts.length-2];
            return evaluate(data, context);
        } catch (err) {
            if (args.length == 2) {
                let onfailure = args[1];
                if (onfailure instanceof Sym) {
                    // Try resolving symbol to function
                    onfailure = self.contexts[self.contexts.length-1].getSym(onfailure.value());
                }
                if (onfailure instanceof Fun) {
                    return onfailure.apply(self, []);
                }
                return onfailure;
            }
            throw new Error(`load: ${err.message}`);
        }
    }));
}

function maybeAddExt(filename) {
    if (path.extname(filename)) {
        return filename;
    }
    return filename + '.lsp';
}

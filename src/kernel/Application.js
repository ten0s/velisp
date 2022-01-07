const fs = require('fs')
const path = require('path')

const {Str, Sym, Fun} = require('../VeLispTypes.js')
const Evaluator = require('../VeLispEvaluator.js')
const {fmtError} = require('../VeLispError.js')
const VeSysInfo = require('../VeSysInfo.js')
const {ensureLspExt} = require('../VeUtil.js')

exports.initContext = (context) => {
    context.setSym('LOAD', new Fun('load', ['filename', '[onfailure]'], [], (self, args) => {
        //console.log('load args:', args)
        if (args.length === 0) {
            throw new Error('load: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('load: too many arguments')
        }
        if (!(args[0] instanceof Str)) {
            throw new Error('load: `filename` expected Str')
        }
        let filename = ensureLspExt(args[0].value())
        // Win32 workaround
        filename = filename.split('\\').join('/')
        if (!path.isAbsolute(filename)) {
            if (!fs.existsSync(filename)) {
                const parent = self.contexts[self.contexts.length-1].getSym('%VELISP_LSP_FILE%')
                if (parent instanceof Str) {
                    filename = path.join(path.dirname(parent.value()), filename)
                }
            }
        }
        try {
            const data = fs.readFileSync(filename).toString()
            // FunCall pushes new context just before the call
            // and pops it after the call.
            // Since (load filename) can defun other functions
            // we need to store them in the parent context.
            const context = self.contexts[self.contexts.length-2]
            const parent = context.getSym('%VELISP_LSP_FILE%')
            context.setSym('%VELISP_LSP_FILE%', new Str(path.resolve(filename)))
            const result = Evaluator.evaluate(data, context)
            // Restore back source file.
            context.setSym('%VELISP_LSP_FILE%', parent)
            return result
        } catch (e) {
            if (args.length === 2) {
                let onfailure = args[1]
                if (onfailure instanceof Sym) {
                    // Try resolving symbol to function
                    onfailure = self.contexts[self.contexts.length-1].getSym(onfailure.value())
                }
                if (onfailure instanceof Fun) {
                    return onfailure.apply(self, [])
                }
                return args[1]
            }
            e.path = filename
            throw new Error(fmtError('load', e))
        }
    }))
    context.setSym('VER', new Fun('ver', [], [], (_self, _args) => {
        return new Str(`${VeSysInfo.name} ${VeSysInfo.version}`)
    }))
}

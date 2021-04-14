const fs = require('fs')
const {Bool, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    // VeLisp Extension
    context.setSym('VE-RMDIR', new Fun('ve-rmdir', ['dirname'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('ve-rmdir: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('ve-rmdir: too many arguments')
        }
        const dirname = ensureType('ve-rmdir:', args[0], [Str]).value()
        try {
            fs.rmdirSync(dirname)
            return new Bool(true)
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
}

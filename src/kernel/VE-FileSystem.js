const fs = require('fs')
const {Bool, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    // VeLisp Extension
    context.setSym('RMDIR', new Fun('rmdir', ['dirname'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('rmdir: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('rmdir: too many arguments')
        }
        const dirname = ensureType('rmdir:', args[0], [Str]).value()
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

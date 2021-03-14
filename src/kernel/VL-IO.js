const fs = require('fs')
const {Bool, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = function (context) {
    context.setSym('VL-FILE-DELETE', new Fun('vl-file-delete', ['filename'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-file-delete: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-file-delete: too many arguments')
        }
        const filename = ensureType('vl-file-delete:', args[0], [Str]).value()
        try {
            fs.unlinkSync(filename)
            return new Bool(true)
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
    context.setSym('VL-MKDIR', new Fun('vl-mkdir', ['dirname'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-mkdir: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-mkdir: too many arguments')
        }
        const dirname = ensureType('vl-mkdir:', args[0], [Str]).value()
        try {
            fs.mkdirSync(dirname)
            return new Bool(true)
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
}

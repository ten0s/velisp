const fs = require('fs')
const {Bool, Int, Str, List, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
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
    context.setSym('VL-FILE-SIZE', new Fun('vl-file-size', ['filename'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-file-size: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-file-size: too many arguments')
        }
        const filename = ensureType('vl-file-size:', args[0], [Str]).value()
        try {
            // TODO: If you do not specify a full path name,
            // vl-file-size searches the AutoCAD default drawing
            // directory for the file.
            const stats = fs.statSync(filename)
            if (stats.isDirectory()) {
                return new Int(0)
            }
            return new Int(stats.size)
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
    context.setSym('VL-FILE-SYSTIME', new Fun('vl-file-systime', ['filename'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-file-systime: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-file-systime: too many arguments')
        }
        const filename = ensureType('vl-file-systime:', args[0], [Str]).value()
        try {
            const stats = fs.statSync(filename)
            if (stats.isDirectory()) {
                return new Bool(false)
            }
            const mtime = stats.mtime
            const year  = new Int(mtime.getFullYear())
            const month = new Int(mtime.getMonth() + 1)
            const dow   = new Int(mtime.getDay())
            const date  = new Int(mtime.getDate())
            const hours = new Int(mtime.getHours())
            const mins  = new Int(mtime.getMinutes())
            const secs  = new Int(mtime.getSeconds())
            const msecs = new Int(mtime.getMilliseconds() - 1)
            return new List([
                year, month, dow, date, hours, mins, secs, msecs
            ])
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

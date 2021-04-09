const fs = require('fs')
const path = require('path')
const temp = require('temp').track()
const {Bool, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = function (context) {
    context.setSym('VL-FILENAME-MKTEMP', new Fun('vl-filename-mktemp', ['pattern', 'directory', 'extention'], [], (self, args) => {
        if (args.length > 3) {
            throw new Error('vl-filename-mktemp: too many arguments')
        }

        const DEFAULT_NAME = '$VL~~'
        const DEFAULT_DIR  = tmpDir()
        const DEFAULT_EXT  = ''

        let dir  = null
        let name = null
        let ext  = null

        if (args.length == 1) {
            let pattern = ensureType('vl-filename-mktemp:', args[0], [Str, Bool])
            if (pattern instanceof Str) {
                ({dir, name, ext} = path.parse(pattern.value()))
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
        }
        if (args.length == 2) {
            let pattern   = ensureType('vl-filename-mktemp:', args[0], [Str, Bool])
            let directory = ensureType('vl-filename-mktemp:', args[1], [Str, Bool])
            if (pattern instanceof Str) {
                ({dir, name, ext} = path.parse(pattern.value()))
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
            if (directory instanceof Str) {
                dir = directory.value()
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
        }
        if (args.length == 3) {
            let pattern   = ensureType('vl-filename-mktemp:', args[0], [Str, Bool])
            let directory = ensureType('vl-filename-mktemp:', args[1], [Str, Bool])
            let extension = ensureType('vl-filename-mktemp:', args[2], [Str, Bool])
            if (pattern instanceof Str) {
                ({dir, name, ext} = path.parse(pattern.value()))
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
            if (directory instanceof Str) {
                dir = directory.value()
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
            if (extension instanceof Str) {
                ext = extension.value()
            }
        }
        // TODO: check out https://www.npmjs.com/package/tmp package
        const {path: tmpFile} = temp.openSync({
            dir   : dir  ? dir  : DEFAULT_DIR,
            prefix: name ? name : DEFAULT_NAME,
            suffix: ext  ? ext  : DEFAULT_EXT,
        })
        return new Str(tmpFile)
    }))
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

const tmpDir = () => {
    let tmp = process.env['TMP']
    if (fs.existsSync(tmp)) { return tmp }
    tmp = process.env['TEMP']
    if (fs.existsSync(tmp)) { return tmp }
    return process.cwd()
}

import fs from 'fs'
import path from 'path'
import temp from 'temp';
import {Bool, Str, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('VL-FILENAME-MKTEMP', new Fun('vl-filename-mktemp', ['pattern', 'directory', 'extention'], [], (self, args) => {
        if (args.length > 3) {
            throw new Error('vl-filename-mktemp: too many arguments')
        }

        const DEFAULT_NAME = 'velisp-'
        const DEFAULT_DIR  = tmpDir()
        const DEFAULT_EXT  = ''

        let dir  = null
        let name = null
        let ext  = null

        // NB: T behaves like nil to not pollute implementation

        if (args.length === 1) {
            let pattern = ensureType('vl-filename-mktemp: `pattern`', args[0], [Str, Bool])
            if (pattern instanceof Str) {
                ({dir, name, ext} = path.parse(pattern.value()))
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
        }
        if (args.length === 2) {
            let pattern   = ensureType('vl-filename-mktemp: `pattern`', args[0], [Str, Bool])
            let directory = ensureType('vl-filename-mktemp: `directory`', args[1], [Str, Bool])
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
        if (args.length === 3) {
            let pattern   = ensureType('vl-filename-mktemp: `pattern`', args[0], [Str, Bool])
            let directory = ensureType('vl-filename-mktemp: `directory`', args[1], [Str, Bool])
            let extension = ensureType('vl-filename-mktemp: `extension`', args[2], [Str, Bool])
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
        const {path: tmpFile} = temp.track().openSync({
            dir   : dir  ? dir  : DEFAULT_DIR,
            prefix: name ? name : DEFAULT_NAME,
            suffix: ext  ? ext  : DEFAULT_EXT,
        })
        return new Str(tmpFile)
    }))
}

const tmpDir = () => {
    const tmp = process.env['TMP']
    if (fs.existsSync(tmp)) { return tmp }
    const temp = process.env['TEMP']
    if (fs.existsSync(temp)) { return temp }
    return process.cwd()
}

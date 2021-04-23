const fs = require('fs')
const VeGlob = require('../VeGlob.js')
const {Bool, Int, Str, List, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    context.setSym('VL-DIRECTORY-FILES', new Fun('vl-directory-files', ['[directory]', '[pattern]', '[what]'], [], (self, args) => {
        if (args.length > 3) {
            throw new Error('vl-directory-files: too many arguments')
        }
        let directory = process.cwd()
        let pattern = '*.*'
        let what = 0
        if (args.length === 1) {
            const dir = ensureType('vl-directory-files: `directory`', args[0], [Str, Bool])
            if (dir instanceof Str) {
                directory = dir.value()
            }
        }
        if (args.length === 2) {
            const dir = ensureType('vl-directory-files: `directory`', args[0], [Str, Bool])
            if (dir instanceof Str) {
                directory = dir.value()
            }
            const pat = ensureType('vl-directory-files: `pattern`', args[1], [Str, Bool])
            if (pat instanceof Str) {
                pattern = pat.value()
            }
        }
        if (args.length === 3) {
            const dir = ensureType('vl-directory-files: `directory`', args[0], [Str, Bool])
            if (dir instanceof Str) {
                directory = dir.value()
            }
            const pat = ensureType('vl-directory-files: `pattern`', args[1], [Str, Bool])
            if (pat instanceof Str) {
                pattern = pat.value()
            }
            const typ = ensureType('vl-directory-files: `what`', args[2], [Int, Bool])
            if (typ instanceof Int) {
                what = typ.value()
            }
        }
        try {
            const glob = new VeGlob(pattern)
            const dirents = [
                new fs.Dirent('.', fs.constants.UV_DIRENT_DIR),
                new fs.Dirent('..', fs.constants.UV_DIRENT_DIR)
            ].concat(fs.readdirSync(directory, {withFileTypes: true}))
            const names = []
            for (const dirent of dirents) {
                const isDir = dirent.isDirectory()
                const isFile = dirent.isFile()
                const name = dirent.name
                const validName = glob.test(name)
                // Dirs only
                if (what < 0 && isDir && validName) {
                    names.push(name)
                }
                // Dirs and files
                if (what === 0 && (isDir || isFile) && validName) {
                    names.push(name)
                }
                // Files only
                if (what > 0 && isFile && validName) {
                    names.push(name)
                }
            }
            return new List(names.map(name => new Str(name)))
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
    context.setSym('VL-FILE-COPY', new Fun('vl-file-copy', ['src-filename', 'dst-filename', '[append]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('vl-file-copy: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('vl-file-copy: too many arguments')
        }
        const srcFilename = ensureType('vl-file-copy: `src-filename`', args[0], [Str]).value()
        const dstFilename = ensureType('vl-file-copy: `dst-filename`', args[1], [Str]).value()
        // TODO: If you do not specify a full path name,
        // vl-file-copy looks the AutoCAD default drawing directory.
        let append = false
        if (args.length === 3) {
            append = ensureType('vl-file-copy: `append`', args[2], [Bool]).value()
        }
        if (srcFilename === dstFilename) {
            return new Bool(false)
        }
        try {
            const srcStats = fs.statSync(srcFilename)
            if (srcStats.isDirectory()) {
                return new Bool(false)
            }
            if (!append) {
                fs.copyFileSync(srcFilename, dstFilename, fs.constants.COPYFILE_EXCL)
            } else {
                const srcBuf = fs.readFileSync(srcFilename)
                const dstFd = fs.openSync(dstFilename, 'a')
                fs.writeSync(dstFd, srcBuf, 0, srcBuf.length)
                fs.closeSync(dstFd)
            }
            return new Int(srcStats.size)
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
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
    context.setSym('VL-FILE-DIRECTORY-P', new Fun('vl-file-directory-p', ['filename'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('vl-file-directory-p: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-file-directory-p: too many arguments')
        }
        const filename = ensureType('vl-file-directory-p:', args[0], [Str]).value()
        // TODO: If you do not specify a full path name,
        // vl-file-directory-p looks the AutoCAD default drawing directory.
        try {
            return new Bool(fs.statSync(filename).isDirectory())
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
    context.setSym('VL-FILE-RENAME', new Fun('vl-file-rename', ['src-filename', 'dst-filename'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('vl-file-rename: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('vl-file-rename: too many arguments')
        }
        const srcFilename = ensureType('vl-file-rename: `src-filename`', args[0], [Str]).value()
        const dstFilename = ensureType('vl-file-rename: `dst-filename`', args[1], [Str]).value()
        // TODO: If you do not specify a full path name,
        // vl-file-rename looks the AutoCAD default drawing directory.
        try {
            // Check destination file exists
            fs.statSync(dstFilename)
            return new Bool(false)
        } catch (e) {
            // fall through
        }
        try {
            fs.renameSync(srcFilename, dstFilename)
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

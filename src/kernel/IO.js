const {Bool, Int, Str, Fun, FileStream, FileMode, File, ensureType} = require('../VeLispTypes.js')

exports.initContext = function (context) {
    context.setSym('PROMPT', new Fun('prompt', ['msg'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('prompt: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('prompt: too many arguments')
        }
        const arg = ensureType('prompt:', args[0], [Str])
        let msg
        if (arg instanceof Str) {
            msg = arg.toUnescapedString()
        } else {
            msg = arg.toString()
        }
        console.log(msg)
        return new Bool(false)
    }))
    context.setSym('PRIN1', new Fun('prin1', ['[expr [file-desc]]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('prin1: too many arguments')
        }
        if (args.length === 0) {
            // TODO: should return some null symbol
            return new Str('')
        }
        const arg = args[0]
        let msg
        // TODO: file-desc
        if (arg instanceof Str) {
            msg = arg.toEscapedString()
        } else {
            msg = arg.toString()
        }
        console.log(msg)
        return arg
    }))
    context.setSym('PRINC', new Fun('princ', ['[expr [file-desc]]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('princ: too many arguments')
        }
        if (args.length === 0) {
            // TODO: should return some null symbol
            return new Str('')
        }
        const arg = args[0]
        let msg
        // TODO: file-desc
        if (arg instanceof Str) {
            msg = arg.toUnescapedString()
        } else {
            msg = arg.toString()
        }
        console.log(msg)
        return arg
    }))
    context.setSym('PRINT', new Fun('print', ['[expr [file-desc]]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('print: too many arguments')
        }
        if (args.length === 0) {
            // TODO: should return some null symbol
            return new Str('')
        }
        const arg = args[0]
        let msg
        // TODO: file-desc
        if (arg instanceof Str) {
            msg = arg.toEscapedString()
        } else {
            msg = arg.toString()
        }
        console.log('\n' + msg + ' ')
        return arg
    }))
    context.setSym('OPEN', new Fun('open', ['filename', 'mode'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('open: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('open: too many arguments')
        }
        const name = ensureType('open: `filename`', args[0], [Str]).value()
        const mode = ensureType('open: `mode`', args[1], [Str]).toLowerCase().value()
        switch (mode) {
        case FileMode.READ:
        case FileMode.WRITE:
        case FileMode.APPEND:
            break
        default:
            throw new Error(`open: unknown mode '${mode}'`)
        }
        return File.open(name, mode)
    }))
    context.setSym('CLOSE', new Fun('close', ['file-desc'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('close: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('close: too many arguments')
        }
        const file = ensureType('close: `file-desc`', args[0], [File])
        return file.close()
    }))
    context.setSym('READ-CHAR', new Fun('read-char', ['file-desc'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('read-char: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('read-char: too many arguments')
        }
        // TODO: Support stdin
        const file = ensureType('read-char: `file-desc`', args[0], [File])
        return file.readChar()
    }))
    context.setSym('READ-LINE', new Fun('read-line', ['file-desc'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('read-line: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('read-line: too many arguments')
        }
        // TODO: Support stdin
        const file = ensureType('read-line: `file-desc`', args[0], [File])
        return file.readLine()
    }))
    context.setSym('WRITE-CHAR', new Fun('write-char', ['num', ['file-desc']], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('write-char: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('write-char: too many arguments')
        }
        const num = ensureType('write-char: `num`', args[0], [Int])
        if (num.value() <= 0 && num.value() > 255) {
            throw new Error('write-char: `num` expected ASCII code')
        }
        let file = null
        if (args.length == 1) {
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
        } else {
            file = ensureType('write-char: `file-desc`', args[1], [File])
        }
        file.writeChar(num)
        return num
    }))
    context.setSym('WRITE-LINE', new Fun('write-line', ['string', ['file-desc']], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('write-line: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('write-line: too many arguments')
        }
        const str = ensureType('write-line: `string`', args[0], [Str])
        let file = null
        if (args.length == 1) {
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
        } else {
            file = ensureType('write-line: `file-desc`', args[1], [File])
        }
        file.writeLine(str)
        return str
    }))
}

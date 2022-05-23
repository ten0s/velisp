import {EOL} from 'os'
import VeSysInfo from '../VeSysInfo.cjs'
import {Bool, Int, Real, Str, Fun, FileStream, FileMode, File, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('GETINT', new Fun('getint', ['[msg]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('getint: too many arguments')
        }
        let msg = ''
        if (args.length === 1) {
            msg = ensureType('getint: `msg`', args[0], [Str]).value()
        }
        let value = undefined
        while (value === undefined) {
            if (msg) {
                const outFile = File.open(FileStream.STDOUT, FileMode.WRITE)
                outFile.write(new Str(msg))
                outFile.close()
            }

            const inFile = File.open(FileStream.STDIN, FileMode.READ)
            const str = inFile.readLine({eol: ' \r\n', echo: VeSysInfo.isRepl}).value()
            inFile.close()

            if (!str) {
                return new Bool(false)
            }

            value = Number.parseInt(str)
            if (!Number.isInteger(value)) {
                value = undefined
                const outFile = File.open(FileStream.STDOUT, FileMode.WRITE)
                outFile.writeLine(new Str('Requires an integer value'))
                outFile.close()
            }
            if (value < -32768 || value > 32767) {
                value = undefined
                const outFile = File.open(FileStream.STDOUT, FileMode.WRITE)
                outFile.writeLine(new Str('Requires an integer between -32768 and 32767'))
                outFile.close()
            }
        }
        return new Int(value)
    }))
    context.setSym('GETREAL', new Fun('getreal', ['[msg]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('getreal: too many arguments')
        }
        let msg = ''
        if (args.length === 1) {
            msg = ensureType('getreal: `msg`', args[0], [Str]).value()
        }
        let value = undefined
        while (value === undefined) {
            if (msg) {
                const outFile = File.open(FileStream.STDOUT, FileMode.WRITE)
                outFile.write(new Str(msg))
                outFile.close()
            }

            const inFile = File.open(FileStream.STDIN, FileMode.READ)
            const str = inFile.readLine({eol: ' \r\n', echo: VeSysInfo.isRepl}).value()
            inFile.close()

            if (!str) {
                return new Bool(false)
            }

            value = Number.parseFloat(str)
            if (!Number.isFinite(value)) {
                value = undefined
                const outFile = File.open(FileStream.STDOUT, FileMode.WRITE)
                outFile.writeLine(new Str('Requires numeric value'))
                outFile.close()
            }
        }
        return new Real(value)
    }))
    context.setSym('GETSTRING', new Fun('getstring', ['[cr]', '[msg]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('getstring: too many arguments')
        }
        let cr = false
        let msg = ''
        if (args.length === 1) {
            const arg = args[0]
            if (arg instanceof Bool) {
                cr = arg.value()
            } else {
                msg = ensureType('getstring: `msg`', arg, [Str]).value()
            }
        }
        if (args.length === 2) {
            cr = ensureType('getstring: `cr`', args[0], [Bool]).value()
            msg = ensureType('getstring: `msg`', args[1], [Str]).value()
        }

        if (msg) {
            const outFile = File.open(FileStream.STDOUT, FileMode.WRITE)
            outFile.write(new Str(msg))
            outFile.close()
        }

        const inFile = File.open(FileStream.STDIN, FileMode.READ)
        const str = inFile.readLine({eol: cr ? '\r\n' : ' \r\n', echo: VeSysInfo.isRepl})
        inFile.close()

        return str
    }))
    context.setSym('PROMPT', new Fun('prompt', ['msg'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('prompt: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('prompt: too many arguments')
        }
        const msg = ensureType('prompt:', args[0], [Str]).toUnescapedString()
        if (msg) {
            const outFile = File.open(FileStream.STDOUT, FileMode.WRITE)
            outFile.write(new Str(msg))
            outFile.close()
        }
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
        const msg = arg.toString()
        let file = undefined
        let close = false
        if (args.length === 1) {
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
            close = true
        } else {
            file = ensureType('prin1: `file-desc`', args[1], [File])
        }
        file.write(new Str(msg))
        if (close) {
            file.close()
        }
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
        let msg = undefined
        if (arg instanceof Str) {
            msg = arg.toUnescapedString()
        } else {
            msg = arg.toString()
        }
        let file = undefined
        let close = false
        if (args.length === 1) {
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
            close = true
        } else {
            file = ensureType('princ: `file-desc`', args[1], [File])
        }
        file.write(new Str(msg))
        if (close) {
            file.close()
        }
        return arg
    }))
    context.setSym('PRINT', new Fun('print', ['[expr [file-desc]]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('print: too many arguments')
        }
        let arg0 = undefined
        let file = undefined
        let close = false
        if (args.length === 0) {
            // TODO: should return some null symbol
            arg0 = new Str('')
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
            close = true
        }
        if (args.length === 1) {
            arg0 = args[0]
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
            close = true
        }
        if (args.length === 2) {
            arg0 = args[0]
            file = ensureType('print: `file-desc`', args[1], [File])
        }
        const msg = arg0.toString()
        file.write(new Str(EOL))
        file.write(new Str(msg))
        file.write(new Str(' '))
        if (close) {
            file.close()
        }
        return arg0
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
        if (args.length > 1) {
            throw new Error('read-char: too many arguments')
        }
        let file = undefined
        let echo = false
        let close = false
        if (args.length === 0) {
            file = File.open(FileStream.STDIN, FileMode.READ)
            echo = VeSysInfo.isRepl
            close = true
        } else {
            file = ensureType('read-char: `file-desc`', args[0], [File])
        }
        const char = file.readChar({echo})
        if (close) {
            file.close()
        }
        return char
    }))
    context.setSym('READ-LINE', new Fun('read-line', ['file-desc'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('read-line: too many arguments')
        }
        let file = undefined
        let echo = false
        let close = false
        if (args.length === 0) {
            file = File.open(FileStream.STDIN, FileMode.READ)
            echo = VeSysInfo.isRepl
            close = true
        } else {
            file = ensureType('read-line: `file-desc`', args[0], [File])
        }
        const line = file.readLine({echo})
        if (close) {
            file.close()
        }
        return line
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
        let file = undefined
        let close = false
        if (args.length == 1) {
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
            close = true
        } else {
            file = ensureType('write-char: `file-desc`', args[1], [File])
        }
        file.writeChar(num)
        if (close) {
            file.close()
        }
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
        let file = undefined
        let close = false
        if (args.length == 1) {
            file = File.open(FileStream.STDOUT, FileMode.WRITE)
            close = true
        } else {
            file = ensureType('write-line: `file-desc`', args[1], [File])
        }
        file.writeLine(str)
        if (close) {
            file.close()
        }
        return str
    }))
}

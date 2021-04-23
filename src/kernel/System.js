const os = require('os')
const {Bool, Int, Str, Fun, ensureType} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    // VeLisp Extension
    context.setSym('CWD', new Fun('cwd', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('cwd: too many arguments')
        }
        return new Str(process.cwd())
    }))
    // VeLisp Extension
    context.setSym('CHDIR', new Fun('chdir', ['dirname'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('chdir: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('chdir: too many arguments')
        }
        const dirname = ensureType('chdir:', args[0], [Str]).value()
        try {
            process.chdir(dirname)
            return new Str(process.cwd())
        } catch (e) {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
    // VeLisp Extension
    context.setSym('HOMEDIR', new Fun('homedir', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('homedir: too many arguments')
        }
        return new Str(os.homedir())
    }))
    // VeLisp Extension
    context.setSym('TMPDIR', new Fun('tmpdir', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('tmpdir: too many arguments')
        }
        return new Str(os.tmpdir())
    }))
    context.setSym('EXIT', new Fun('exit', ['[code]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('exit: too many arguments')
        }
        let code = 0
        if (args.length === 1) {
            code = ensureType('exit:', args[0], [Int]).value()
        }
        process.exit(code)
    }))
    context.setSym('GETENV', new Fun('getenv', ['name'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('getenv: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('getenv: too many arguments')
        }
        const name = ensureType('getenv:', args[0], [Str])
        const value = process.env[name.value()]
        if (value === undefined) {
            return new Bool(false)
        }
        return new Str(value)
    }))
    context.setSym('QUIT', new Fun('quit', ['[code]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('quit: too many arguments')
        }
        let code = 0
        if (args.length === 1) {
            code = ensureType('quit:', args[0], [Int]).value()
        }
        process.exit(code)
    }))
    context.setSym('SETENV', new Fun('setenv', ['name', 'value'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('setenv: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('setenv: too many arguments')
        }
        const name = ensureType('setenv: `name`', args[0], [Str])
        const value = ensureType('setenv: `value`', args[1], [Str])
        process.env[name.value()] = value.value()
        return value
    }))
}

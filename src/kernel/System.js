import os from 'os'
import {spawn} from 'child_process'
import {Bool, Int, Real, Str, Sym, List, Fun, ensureType} from '../VeLispTypes.js'
import {homeDir, tmpDir} from '../VeSystem.js'

export const initContext = (context) => {
    // VeLisp Extension
    context.setSym('ARGV', new Fun('argv', ['[n]'], [], (self, args) => {
        // devel mode  : $ node src/main.js test.js 1 two
        // release mode: $ velisp test.js 1 two
        // ("test.js" "1" "two")
        // stdin mode  : $ velisp -- 1 two
        // tty mode    : $ cat test.js | velisp -- 1 two
        // ("--" "1" "two")
        if (args.length > 1) {
            throw new Error('argv: too many arguments')
        }
        const argv = [...process.argv].slice(2)
        if (args.length === 1) {
            const n = ensureType('argv:', args[0], [Int]).value()
            if (n < 0) {
                throw new Error('argv: expected positive Int')
            }
            const arg = argv[n]
            if (arg) {
                return new Str(arg)
            } else {
                return new Bool(false)
            }
        }
        return new List(argv.map(s => new Str(s)))
    }))
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
        return new Str(homeDir())
    }))
    // VeLisp Extension
    context.setSym('TMPDIR', new Fun('tmpdir', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('tmpdir: too many arguments')
        }
        return new Str(tmpDir())
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
        const name = ensureType('getenv:', args[0], [Str]).value()
        const value = process.env[name]
        if (value === undefined) {
            switch (name) {
            // Linux doesn't have TMP and TEMP env vars
            case 'TMP':
            case 'TEMP':
                return new Str(tmpDir())
            default:
                return new Bool(false)
            }
        }
        return new Str(value)
    }))
    context.setSym('GETVAR', new Fun('getvar', ['name'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('getvar: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('getvar: too many arguments')
        }
        const name = ensureType('getvar:', args[0], [Str, Sym]).value().toLowerCase()
        switch (name) {
        case 'cdate': {
            const d = new Date()
            const a = d.getFullYear() * 10000 + d.getMonth() * 100 + d.getDate()
            const b = d.getHours() * 10000 + d.getMinutes() * 100 + d.getSeconds()
            return new Real(a + b / 1000000)
        }
        case 'millisecs':
            return new Int(os.uptime() * 1000)
        default:
            return new Bool(false)
        }
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
    context.setSym('SLEEP', new Fun('sleep', ['millisecs'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('sleep: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('sleep: too many arguments')
        }
        const msecs = ensureType('sleep:', args[0], [Int]).value()
        if (msecs < 0) {
            throw new Error('sleep: expected positive Int')
        }
        // Thanks to https://www.npmjs.com/package/sleep
        Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, msecs)
        return new Bool(false)
    }))
    context.setSym('STARTAPP', new Fun('startapp', ['cmd', '[file]'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('startapp: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('startapp: too many arguments')
        }
        const cmd = ensureType('startapp: `cmd`', args[0], [Str]).value()
        let file = undefined
        if (args.length === 2) {
            file = ensureType('startapp: `file`', args[1], [Str]).value()
        }
        const child = spawn(cmd, file ? [file] : [], {
            detached: true,
            stdio: 'ignore',
            windowsHide: true,
        }).on('error', () => {})
        child.unref()
        if (child.pid) {
            return new Int(child.pid)
        }
        return new Bool(false)
    }))
}

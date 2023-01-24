/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2023 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

/* SPDX-License-Identifier: GPL-3.0-or-later */

import os from 'os'
import {spawn} from 'child_process'
import {Bool, Int, Real, Str, Sym, List, KFun, Argv0, ensureType} from '../VeLispTypes.js'
import VeArgv from '../VeArgv.js'
import {homeDir, tmpDir, sleep} from '../VeSystem.js'
import VeSysInfo from '../VeSysInfo.js'

export const initContext = (context) => {
    // VeLisp Extension
    context.setSym('ARGV', new KFun('argv', ['[n]'], [], (self, args) => {
        // devel mode  : $ node src/main.js [--no-dcl] test.js 1 two
        // release mode: $ velisp [--no-dcl] test.js 1 two
        // (#<argv0> "test.js" "1" "two")
        // stdin mode  : $ velisp [--no-dcl] -- 1 two
        // tty mode    : $ cat test.js | velisp [--no-dcl] -- 1 two
        // eval mode   : $ velisp [--no-dcl] --eval '(argv)' -- 1 two
        // (#<argv0> "--" "1" "two")
        if (args.length > 1) {
            throw new Error('argv: too many arguments')
        }
        const argv0 = VeArgv.lspArgv0(process.argv)
        const argv = VeArgv.lspArgv(process.argv)
        if (args.length === 1) {
            const n = ensureType('argv:', args[0], [Int]).value()
            if (n < 0) {
                throw new Error('argv: expected positive Int')
            }
            if (n === 0) {
                return new Argv0(argv0)
            }
            const arg = argv[n-1]
            if (arg) {
                return new Str(arg)
            } else {
                return new Bool(false)
            }
        }
        return new List(
            [new Argv0(argv0)].concat(
                argv.map(s => new Str(s))
            )
        )
    }))
    // VeLisp Extension
    context.setSym('CWD', new KFun('cwd', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('cwd: too many arguments')
        }
        return new Str(process.cwd())
    }))
    // VeLisp Extension
    context.setSym('CHDIR', new KFun('chdir', ['dirname'], [], (self, args) => {
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
    context.setSym('HOMEDIR', new KFun('homedir', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('homedir: too many arguments')
        }
        return new Str(homeDir())
    }))
    // VeLisp Extension
    context.setSym('TMPDIR', new KFun('tmpdir', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('tmpdir: too many arguments')
        }
        return new Str(tmpDir())
    }))
    context.setSym('EXIT', new KFun('exit', ['[code]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('exit: too many arguments')
        }
        let code = 0
        if (args.length === 1) {
            code = ensureType('exit:', args[0], [Int]).value()
        }
        process.exit(code)
    }))
    context.setSym('GETENV', new KFun('getenv', ['name'], [], (self, args) => {
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
    context.setSym('GETVAR', new KFun('getvar', ['name'], [], (self, args) => {
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
        case 'velisp-version':
            return new Str(VeSysInfo.version)
        case 'velisp-file': {
            const file = self.stack.top().callerFile
            if (typeof file === 'string') {
                return new Str(file)
            }
            return new Bool(false)
        }
        case 'velisp-line': {
            const line = self.stack.top().callerLine
            if (typeof line === 'number') {
                return new Int(line)
            }
            return new Bool(false)
        }
        case 'velisp-function': {
            const funName = self.stack.top(1).funName
            if (typeof funName === 'string') {
                return new Str(funName)
            }
            return new Bool(false)
        }
        default:
            return new Bool(false)
        }
    }))
    context.setSym('QUIT', new KFun('quit', ['[code]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('quit: too many arguments')
        }
        let code = 0
        if (args.length === 1) {
            code = ensureType('quit:', args[0], [Int]).value()
        }
        process.exit(code)
    }))
    context.setSym('SETENV', new KFun('setenv', ['name', 'value'], [], (self, args) => {
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
    context.setSym('SLEEP', new KFun('sleep', ['millisecs'], [], (self, args) => {
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
        sleep(msecs)
        return new Bool(false)
    }))
    context.setSym('STARTAPP', new KFun('startapp', ['cmd', '[arg ...]'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('startapp: too few arguments')
        }
        let allArgs
        if (args[0] instanceof Argv0) {
            const argv0 = args[0].value()
            allArgs = [...argv0]
        } else if (args[0] instanceof Str) {
            const arg = args[0].value()
            allArgs = [arg]
        } else {
            throw new Error('startapp: `cmd` expected Str, Argv0')
        }
        if (args.length > 1) {
            for (let i = 1; i < args.length; i++) {
                const arg = ensureType('startapp: `arg`', args[i], [Str]).value()
                allArgs.push(arg)
            }
        }
        let cmd
        let cmdArgs
        let windowsHide = false
        let windowsVerbatimArguments = false
        if (process.platform === 'win32') {
            if (args[0] instanceof Argv0) {
                // Windows 10 doesn't allow starting unknown programs in
                // the hidden state. We trick it here by first running
                // something it knows really well: cmd.exe /C.
                // > cmd.exe /C ""C:\\path to\\velisp.exe" "arg1" "arg 2""
                // The same trick is used in windows/noprompt.vbs
                cmd = process.env.comspec || 'cmd.exe'
                cmdArgs = ['/C']
                const quote = (s) => `"${s}"`
                cmdArgs.push(`"${allArgs.map(quote).join(' ')}"`)
                windowsHide = true
                windowsVerbatimArguments = true
            } else {
                cmd = allArgs.shift()
                cmdArgs = allArgs
            }
        } else {
            cmd = allArgs.shift()
            cmdArgs = allArgs
        }
        //console.log('spawn', cmd, cmdArgs)
        const child = spawn(cmd, cmdArgs, {
            detached: false,
            stdio: 'inherit',
            windowsHide,
            windowsVerbatimArguments,
        }).on('error', () => {})
        child.unref()
        if (child.pid) {
            return new Int(child.pid)
        }
        return new Bool(false)
    }))
}

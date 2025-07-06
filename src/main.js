/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2020-2025 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

import fs from 'fs'
import os from 'os'
import path from 'path'
import repl from 'repl'
import {Readable} from 'stream'
import {Command} from 'commander'

import __rootdir from './VeRootDir.js'
import VeArgv from './VeArgv.js'
import VeSysInfo from './VeSysInfo.js'
import {
    ensureLspExt,
    inspect,
    isRecoverableInput,
    makeUnixPath,
    makeWinPath,
} from './VeUtil.js'
import VeLispContext from './VeLispContext.js'
import VeLispContextIniter from './VeLispContextIniter.js'
import VeStack from './VeStack.js'
import {evaluate, tree} from './VeLispEvaluator.js'
import {fmtError, catchError, printError} from './VeLispError.js'
import {Bool} from './VeLispTypes.js'
import {EXIT_SUCCESS} from './VeConst.js'

main()

function main() {
    parseDebugEnv(VeSysInfo.debug)
    adjustEnvVars()
    const initArgv = VeArgv.initArgv(process.argv)
    const program = addCommandOptions(new Command())
    program.version(versionInfo(VeSysInfo) + '\n' + licenseInfo())
        .arguments('[file]')
        .action(async (file) => {
            const options = program.opts()
            const action = runAction(VeSysInfo.debug)

            const stack = new VeStack()
            stack.push(new VeLispContext())

            VeLispContextIniter.initWithKernel(stack)
            await maybeInjectDcl(options.dcl, action, stack)
            maybeInjectLib(action, stack)

            if (options.eval) {
                //console.log(`Eval from ${options.eval}`);
                const file = path.join(process.cwd(), '__EVAL__')
                stack.top().callerFile = file
                readStream(Readable.from(options.eval), action, stack)
            } else if (file) {
                //console.log(`Read from ${file}`);
                file = ensureLspExt(path.resolve(makeUnixPath(file)))
                stack.top().callerFile = file
                readStream(fs.createReadStream(file), action, stack)
            } else if (process.stdin.isTTY) {
                //console.log('Read from tty');
                VeSysInfo.isRepl = true
                const file = path.join(process.cwd(), '__REPL__')
                stack.top().callerFile = file
                startRepl(action, stack)
            } else {
                //console.log('Read from stdin');
                const file = path.join(process.cwd(), '__STDIN__')
                stack.top().callerFile = file
                readStream(process.stdin, action, stack)
            }
        })
        .parse(initArgv)
}

function parseDebugEnv(debug) {
    const what = (process.env['VELISP_DEBUG'] || '').trim().toLowerCase()
    if (what) {
        if (debug.hasOwnProperty(what)) {
            debug[what] = true
        } else {
            console.error(`Warning: debug option \`${what}\` is unknown; try VELISP_DEBUG=help`)
        }
    }
}

function adjustEnvVars() {
    setRootDirEnvVar()
    if (process.platform === 'win32') {
        winAddMingw64ToPATH()
    }
}

function setRootDirEnvVar() {
    let rootdir = makeUnixPath(__rootdir)
    if (rootdir.includes('snapshot')) {
        // https://github.com/vercel/pkg#snapshot-filesystem
        rootdir = makeUnixPath(path.dirname(process.argv[0]))
    }
    process.env['VELISP_ROOT'] = rootdir
}

function winAddMingw64ToPATH() {
    const pathDirs = process.env['PATH']
    const rootDir = process.env['VELISP_ROOT']
    const mingwDir = makeWinPath(`${rootDir}/mingw64/bin`)
    process.env['PATH'] = `${mingwDir};${pathDirs}`
}

function addCommandOptions(command) {
    VeArgv.options().forEach(o => {
        command.option(o.option, o.help, o.default)
    })

    return command
}

function runAction(debug) {
    switch (true) {
    case debug.help:
        console.error(debugHelp())
        process.exit(EXIT_SUCCESS)
        return
    case debug.tree:
        return (input, stack) => {
            console.log(tree(input, stack))
        }
    default:
        return evaluate
    }
}

function maybeInjectLib(action, stack) {
    if (action === evaluate) {
        VeLispContextIniter.initWithLib(stack)
    }
}

async function maybeInjectDcl(withDcl, action, stack) {
    VeSysInfo.withDcl = withDcl
    if (action === evaluate && withDcl) {
        await VeLispContextIniter.initWithDcl(stack)
    }
    stack.top().setSym('%VELISP_DCL%', new Bool(withDcl))
}

function readStream(stream, action, stack) {
    let input = ''
    stream.on('data', (chunk) => {
        input += chunk.toString()
    })
    stream.on('end', () => {
        catchError(
            () => {
                if (input.trim()) {
                    action(input, stack)
                }
            },
            printError,
            stack
        )
    })
    stream.on('error', (e) => {
        printError(new Error(fmtError('open', e)), stack)
    })
}

function startRepl(action, stack) {
    console.log(versionInfo(VeSysInfo))
    console.log('Type ".license" or ".help" for more information')

    let historyFile = process.env['VELISP_REPL_HISTORY']
    if (historyFile !== '') { // Is enabled?
        if (historyFile === undefined) {
            historyFile = path.join(os.homedir(), '.velisp_repl_history')
        }
    }
    const HISTORY_SIZE = 1000
    let historySize = process.env['VELISP_REPL_HISTORY_SIZE']
    if (!historySize) {
        historySize = HISTORY_SIZE
    } else {
        historySize = Number.parseInt(historySize)
        if (!Number.isInteger(historySize) || historySize <= 0) {
            console.error(`Error: VELISP_REPL_HISTORY_SIZE is invalid. Use ${HISTORY_SIZE}`)
            historySize = HISTORY_SIZE
        }
    }

    const replServer = repl.start({
        prompt: VeSysInfo.prompt,
        useGlobal: true,
        historySize: historySize,
        eval: (input, replCtx, filename, callback) => {
            return replEval(repl, input, action, stack, callback)
        },
        completer: (line) => {
            return replCompleter(repl, line, stack)
        },
        writer: (output) => {
            return replWriter(repl, output)
        }
    })
    if (action === evaluate) {
        replServer.defineCommand('context', {
            help: 'Show global context',
            action() {
                console.log(stack.top())
                this.displayPrompt()
            }
        })
        replServer.defineCommand('inspect', {
            help: 'Inspect internal representation of <expression>',
            action(input) {
                if (input.trim()) {
                    catchError(
                        () => {
                            const result = action(input, stack)
                            if (result !== null) {
                                console.log(inspect(result))
                            }
                        },
                        printError,
                        stack
                    )
                    // Leave only main context in stack
                    stack.unwind()
                    // Fall through
                }
                this.displayPrompt()
            }
        })
        replServer.defineCommand('license', {
            help: 'Show license info',
            action() {
                console.log(licenseInfo())
                this.displayPrompt()
            }
        })
        if (historyFile !== '') { // Is enabled?
            replServer.setupHistory(historyFile, (err, _repl) => {
                if (err) {
                    console.error(err)
                }
            })
        }
    }
}

function replEval(repl, input, action, stack, callback) {
    if (input.trim()) {
        try {
            const result = action(input, stack)
            if (result !== null && result !== undefined) {
                return callback(null, result)
            }
        } catch (e) {
            if (isRecoverable(input, e)) {
                return callback(new repl.Recoverable(e))
            } else {
                printError(e, stack)
                // Leave only main context in stack
                stack.unwind()
                // Fall through
            }
        }
    }
    callback(null)
}

function replCompleter(_repl, line, stack) {
    const forms = [
        '.break', '.clear', '.context', '.editor',
        '.exit', '.help', '.inspect', '.load', '.save',

        'and', 'cond', 'defun', 'foreach', 'if',
        'lambda', 'or', 'progn', 'quote', 'repeat',
        'setq', 'while', 'list', 'nil'
    ]
    const symbols = Object.keys(stack.top().symbols).map(s => s.toLowerCase())
    const completions = forms.concat(symbols)
    const tokens = line.split(' ')
    if (tokens.length) {
        const last = tokens[tokens.length-1].replace('(', '')
        const hits = completions.filter(c => c.startsWith(last))
        return [hits.length ? hits : completions, last]
    }
    return [completions, line]
}

function replWriter(_repl, output) {
    // TODO: Types come in, color them appropriately
    return output
}

function isRecoverable(input, _error) {
    return isRecoverableInput(input)
}

function versionInfo(info) {
    return `${info.name} ${info.version} on ${info.platform}`
}

function licenseInfo() {
    return 'Copyright (C) 2020-2025 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>    \n' +
           'License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n' +
           'This is free software: you are free to change and redistribute it.           \n' +
           'There is NO WARRANTY, to the extent permitted by law.'
}

function debugHelp() {
    return 'Valid options for the VELISP_DEBUG environment variable are:\n' +
           '  glade       show Glade XML                                \n' +
           '  fulltrace   show full stacktrace                          \n' +
           '  tree        show parse tree                               \n' +
           '  libs        show libs loading (MacOS)                     \n' +
           '  help        show this help message and exit               \n'
}

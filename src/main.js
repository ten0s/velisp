/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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
import {evaluate, tree} from './VeLispEvaluator.js'
import {fmtError} from './VeLispError.js'
import {Bool, Str} from './VeLispTypes.js'

main()

function main() {
    parseDebugEnv(VeSysInfo.debug)
    adjustEnvVars()
    const initArgv = VeArgv.initArgv(process.argv)
    const program = addCommandOptions(new Command())
    program.version(versionInfo() + '\n' + licenseInfo())
        .arguments('[file]')
        .action(async (file) => {
            const options = program.opts()
            const action = runAction(VeSysInfo.debug)

            const context = new VeLispContext()
            VeLispContextIniter.initWithKernel(context)
            await maybeInjectDcl(action, options.dcl, context)
            maybeInjectLib(action, context)

            if (options.eval) {
                //console.log(`Eval from ${options.eval}`);
                const file = path.join(process.cwd(), '__EVAL__')
                context.setSym('%VELISP_LSP_FILE%', new Str(file))
                readStream(Readable.from(options.eval), action, context)
            } else if (file) {
                //console.log(`Read from ${file}`);
                file = ensureLspExt(path.resolve(makeUnixPath(file)))
                context.setSym('%VELISP_LSP_FILE%', new Str(file))
                readStream(fs.createReadStream(file), action, context)
            } else if (process.stdin.isTTY) {
                //console.log('Read from tty');
                VeSysInfo.isRepl = true
                const file = path.join(process.cwd(), '__REPL__')
                context.setSym('%VELISP_LSP_FILE%', new Str(file))
                startRepl(action, context)
            } else {
                //console.log('Read from stdin');
                const file = path.join(process.cwd(), '__STDIN__')
                context.setSym('%VELISP_LSP_FILE%', new Str(file))
                readStream(process.stdin, action, context)
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
        process.exit(0)
        return
    case debug.tree:
        return (input, context) => {
            console.log(tree(input, context))
        }
    default:
        return evaluate
    }
}

function maybeInjectLib(action, context) {
    if (action === evaluate) {
        VeLispContextIniter.initWithLib(context)
    }
}

async function maybeInjectDcl(action, withDcl, context) {
    VeSysInfo.withDcl = withDcl
    if (action === evaluate && withDcl) {
        await VeLispContextIniter.initWithDcl(context)
    }
    context.setSym('%VELISP_DCL%', new Bool(withDcl))
}

function printError(e) {
    if (VeSysInfo.debug.stacktrace) {
        console.error(e)
    } else {
        console.error(`Error: ${e.message}`)
    }
}

function readStream(stream, action, context) {
    let input = ''
    stream.on('data', (chunk) => {
        input += chunk.toString()
    })
    stream.on('end', () => {
        try {
            if (input.trim()) {
                action(input, context)
            }
        } catch (e) {
            printError(e)
        }
    })
    stream.on('error', (e) => {
        printError(new Error(fmtError('open', e)))
    })
}

function startRepl(action, context) {
    console.log(versionInfo())
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
        prompt: '> ',
        useGlobal: true,
        historySize: historySize,
        eval: (input, replCtx, filename, callback) => {
            return replEval(repl, input, action, context, callback)
        },
        completer: (line) => {
            return replCompleter(repl, line, context)
        },
        writer: (output) => {
            return replWriter(repl, output)
        }
    })
    if (action === evaluate) {
        replServer.defineCommand('context', {
            help: 'Show global context',
            action() {
                console.log(context)
                this.displayPrompt()
            }
        })
        replServer.defineCommand('inspect', {
            help: 'Inspect internal representation of <expression>',
            action(input) {
                if (input.trim()) {
                    try {
                        const result = action(input, context)
                        if (result !== null) {
                            console.log(inspect(result))
                        }
                    } catch (e) {
                        printError(e)
                        // fall through
                    }
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

function replEval(repl, input, action, context, callback) {
    if (input.trim()) {
        try {
            const result = action(input, context)
            if (result !== null && result !== undefined) {
                return callback(null, result)
            }
        } catch (e) {
            if (isRecoverable(input, e)) {
                return callback(new repl.Recoverable(e))
            } else {
                printError(e)
                // fall through
            }
        }
    }
    callback(null)
}

function replCompleter(_repl, line, context) {
    const forms = [
        '.break', '.clear', '.context', '.editor',
        '.exit', '.help', '.inspect', '.load', '.save',

        'and', 'cond', 'defun', 'foreach', 'if',
        'lambda', 'or', 'progn', 'quote', 'repeat',
        'setq', 'while', 'list', 'nil'
    ]
    const symbols = Object.keys(context.symbols).map(s => s.toLowerCase())
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

function versionInfo() {
    return `${VeSysInfo.name} ${VeSysInfo.version} on ${VeSysInfo.platform}`
}

function licenseInfo() {
    return 'Copyright (C) 2022 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>         \n' +
           'License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n' +
           'This is free software: you are free to change and redistribute it.           \n' +
           'There is NO WARRANTY, to the extent permitted by law.'
}

function debugHelp() {
    return 'Valid options for the VELISP_DEBUG environment variable are:\n' +
           '  glade       show Glade XML                                \n' +
           '  stacktrace  show full stacktrace                          \n' +
           '  tree        show parse tree                               \n' +
           '  help        show this help message and exit               \n'
}

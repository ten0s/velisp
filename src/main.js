import fs from 'fs'
import os from 'os'
import path from 'path'
import repl from 'repl'
import {Readable} from 'stream'
import {Command} from 'commander'

import __rootdir from './VeRootDir.js'
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
import {Bool, Str} from './VeLispTypes.js'

main()

function main() {
    adjustEnvVars()
    const [init, ] = preProcessArgv()
    const program = addCommandOptions(new Command())
    program.version(VeSysInfo.version)
        .arguments('[file]')
        .action(async (file) => {
            const options = program.opts()
            const action = runAction(options)

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
        .parse(init)
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

function preProcessArgv() {
    const argv = process.argv
    let init = [...argv]
    let rest = []
    const i = argv.indexOf('-')
    const j = argv.indexOf('--')
    const k = i !== -1 ? i : j
    if (k !== -1) {
        init = argv.slice(0, k)
        rest = argv.slice(k)
    }
    return [init, rest]
}

function addCommandOptions(command) {
    const opts = [
        {short: '-e', full: '--eval <expr>', help: 'evaluate script', default: ''},
        {short: ''  , full: '--no-dcl'     , help: 'run without dcl', default: true},
        {short: ''  , full: '--tree'       , help: 'see parse tree' , default: false},
    ]

    opts.forEach(opt => {
        const option = opt.short ? `${opt.short}, ${opt.full}` : opt.full
        command.option(option, opt.help, opt.default)
    })

    return command
}

function runAction(options) {
    if (options.tree) {
        return (input, context) => {
            console.log(tree(input, context))
        }
    }

    return evaluate
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
            console.error(e)
        }
    })
    stream.on('error', (e) => {
        console.error(e)
    })
}

function startRepl(action, context) {
    console.log(`${VeSysInfo.name} ${VeSysInfo.version} on ${process.platform}`)
    console.log('Type ".help" for more information')

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
                        console.error(e)
                        // fall through
                    }
                }
                this.displayPrompt()
            }
        })
        if (historyFile !== '') { // Is enabled?
            replServer.setupHistory(historyFile, (err, _repl) => {
                if (err) {
                    console.log(err)
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
                console.error(e)
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

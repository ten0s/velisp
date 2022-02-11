const fs = require('fs')
const os = require('os')
const path = require('path')
const {Command} = require('commander')

const VeSysInfo = require('./VeSysInfo.js')
const {ensureLspExt, inspect, fixWinPath} = require('./VeUtil.js')
const VeLispGlobalContext = require('./VeLispGlobalContext.js')
const {evaluate, tree} = require('./VeLispEvaluator.js')
const {Str} = require('./VeLispTypes.js')

main()

function main() {
    const [init, ] = preProcessArgv()
    const program = new Command()
    program.version(VeSysInfo.version)
        .option('-r, --run <command>', 'eval | tree', 'eval')
        .arguments('[file]')
        .action((file) => {
            const action = runAction(program.run)
            const context = new VeLispGlobalContext()
            maybeInjectLib(action, context)
            if (file) {
                //console.log(`Read from ${file}`);
                file = ensureLspExt(path.resolve(fixWinPath(file)))
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

function runAction(what, isRepl) {
    switch (what.toLowerCase()) {
    case 'eval':
        return evaluate
    case 'tree':
        // Repl prints itself
        if (isRepl) {
            return tree
        }
        // Let's print for others
        return (input, context) => {
            console.log(tree(input, context))
        }
    default:
        console.error(`Unknown action: ${what}`)
        process.exit(1)
    }
}

function maybeInjectLib(action, context) {
    if (action === evaluate) {
        let rootdir = fixWinPath(path.join(__dirname, '..'))
        process.env['VELISP_ROOT'] = rootdir
        evaluate(`(load "${rootdir}/lib/main.lsp")`, context)
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

    const repl = require('repl')
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
    const {isRecoverableInput} = require('./VeUtil.js')
    return isRecoverableInput(input)
}

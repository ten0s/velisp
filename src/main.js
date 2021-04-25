const os = require('os')
const path = require('path')
const {Command} = require('commander')
const VeLispGlobalContext = require('./VeLispGlobalContext.js')
const {evaluate, tree} = require('./VeLispEvaluator.js')
const config = require('../package.json')

main()

function main() {
    const program = new Command()
    program.version(config.version)
        .option('-r, --run <command>', 'eval | tree', 'eval')
        .arguments('[file]')
        .action((file) => {
            const action = runAction(program.run)
            const context = new VeLispGlobalContext()
            maybeInjectLib(action, context)
            if (file) {
                //console.log(`Read from ${file}`);
                const fs = require('fs')
                readStream(fs.createReadStream(file), action, context)
            } else if (process.stdin.isTTY) {
                //console.log('Read from tty');
                startRepl(config, action, context)
            } else {
                //console.log('Read from stdin');
                readStream(process.stdin, action, context)
            }
        })
        .parse(process.argv)
}

function runAction(what, isREPL) {
    switch (what.toLowerCase()) {
    case 'eval':
        return evaluate
    case 'tree':
        // REPL prints itself
        if (isREPL) {
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
        let rootdir = path.join(__dirname, '..')
        // Win32 workaround
        rootdir = rootdir.split('\\').join('/')
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

function startRepl(config, action, context) {
    console.log(`${config.name} ${config.version} on ${process.platform}`)
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
                            console.log(result)
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
    const symbols = Object.keys(context.symbols).map(s => s.toLowerCase())
    const tokens = line.split(' ')
    if (tokens.length) {
        const last = tokens[tokens.length-1].replace('(', '')
        const hits = symbols.filter(c => c.startsWith(last))
        return [hits.length ? hits : symbols, last]
    }
    return [symbols, line]
}

function replWriter(_repl, output) {
    // TODO: Types come in, color them appropriately
    return output
}

function isRecoverable(input, _error) {
    const {isRecoverableInput} = require('./VeUtil.js')
    return isRecoverableInput(input)
}

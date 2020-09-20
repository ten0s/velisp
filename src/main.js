const {Command} = require('commander');
const {VeLispGlobalContext} = require('./VeLispGlobalContext.js');
const {evaluate, tree} = require('./VeLispEvaluator.js');
const config = require('../package.json');

const program = new Command();
program.version(config.version)
    .option('-r, --run <command>', 'eval | tree', 'eval')
    .arguments('[file]')
    .action((file) => {
        const action = runAction(program.run);
        const context = new VeLispGlobalContext();
        maybeInjectStdLib(action, context);
        if (file) {
            //console.log(`Read from ${file}`);
            const fs = require('fs');
            readStream(fs.createReadStream(file), action, context);
        } else if (process.stdin.isTTY) {
            //console.log('Read from tty');
            startRepl(config, action, context);
        } else {
            //console.log('Read from stdin');
            readStream(process.stdin, action, context);
        }
    })
    .parse(process.argv);

function runAction(what, isREPL) {
    switch (what.toLowerCase()) {
    case 'eval':
        return evaluate;
    case 'tree':
        // REPL prints itself
        if (isREPL) {
            return tree;
        }
        // Let's print for others
        return (input, context) => {
            console.log(tree(input, context))
        };
    default:
        console.error(`Unknown action: ${what}`);
        process.exit(1);
    }
}

function maybeInjectStdLib(action, context) {
    if (action === evaluate) {
        const path = require('path');
        const rootdir = path.join(__dirname, '..');
        process.env['VELISP_ROOT'] = rootdir;
        evaluate(`(load "${rootdir}/lib/stdlib/main.lsp")`, context);
    }
}

function readStream(stream, action, context) {
    let input = "";
    stream.on('data', (chunk) => {
        input += chunk.toString();
    });
    stream.on('end', () => {
        try {
            if (input.trim()) {
                action(input, context);
            }
        } catch (e) {
            console.error(e.message);
        }
    });
    stream.on('error', (e) => {
        console.error(e.message);
    });
}

function startRepl(config, action, context) {
    console.log(`${config.name} ${config.version} on ${process.platform}`);
    console.log('Type ".help" for more information');
    const repl = require('repl');
    const replServer = repl.start({
        prompt: '> ',
        eval: (input, replCtx, filename, callback) => {
            return replEval(repl, input, action, context, callback);
        },
        writer: (output) => {
            return replWriter(repl, output);
        }
    });
    if (action === evaluate) {
        replServer.defineCommand('context', {
            help: 'Show global context',
            action() {
                console.log(context);
                this.displayPrompt();
            }
        });
        replServer.defineCommand('type', {
            help: 'Show expression\'s internal type',
            action(input) {
                try {
                    if (input.trim()) {
                        console.log(action(input, context));
                    }
                } catch (e) {
                    console.error(e.message);
                }
                this.displayPrompt();
            }
        });
    }
}

function replEval(repl, input, action, context, callback) {
    if (input.trim()) {
        try {
            return callback(null, action(input, context));
        } catch (e) {
            console.error(e.message);
            // fall through
        }
    }
    callback(null);
}

function replWriter(repl, output) {
    // TODO: Types come in, color them appropriately
    return output;
}

const {Command} = require('commander');
const {VeLispGlobalContext} = require('./VeLispGlobalContext.js');
const {evaluate} = require('./VeLispEvaluator.js');
const config = require('../package.json');

const program = new Command();
program.version(config.version)
    .arguments('[file]')
    .action((file) => {
        const context = new VeLispGlobalContext();
        if (file) {
            //console.log(`Read from ${file}`);
            const fs = require('fs');
            readStream(fs.createReadStream(file), context);
        } else if (process.stdin.isTTY) {
            //console.log('Read from tty');
            startRepl(config, context);
        } else {
            //console.log('Read from stdin');
            readStream(process.stdin, context);
        }
    })
    .parse(process.argv);

function readStream(stream, context) {
    let input = "";
    stream.on('data', (chunk) => {
        input += chunk.toString();
    });
    stream.on('end', () => {
        try {
            if (input.trim()) {
                evaluate(input, context);
            }
        } catch (e) {
            console.error(e.message);
        }
    });
    stream.on('error', (e) => {
        console.error(e.message);
    });
}

function startRepl(config, context) {
    console.log(`${config.name} ${config.version} on ${process.platform}`);
    console.log('Type ".help" for more information');
    const repl = require('repl');
    const replServer = repl.start({
        prompt: '> ',
        eval: (input, replCtx, filename, callback) => {
            return replEval(repl, input, context, callback);
        },
        writer: (output) => {
            return replWriter(repl, output);
        }
    });
    replServer.defineCommand('type', {
        help: 'Evaluate expression and show its internal type',
        action(input) {
            try {
                if (input.trim()) {
                    console.log(evaluate(input, context));
                }
            } catch (e) {
                console.error(e.message);
            }
            this.displayPrompt();
        }
    });
}

function replEval(repl, input, context, callback) {
    if (input.trim()) {
        try {
            return callback(null, evaluate(input, context));
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

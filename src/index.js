const fs = require('fs');
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
            read_stream(fs.createReadStream(file), context);
        } else if (process.stdin.isTTY) {
            //console.log('Read from tty');
            start_repl(context);
        } else {
            //console.log('Read from stdin');
            read_stream(process.stdin, context);
        }
    })
    .parse(process.argv);

function read_stream(stream, context) {
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

function start_repl(context) {
    const repl = require('repl');
    repl.start({
        prompt: '> ',
        eval: (input, replCtx, filename, callback) => {
            if (input.trim()) {
                callback(null, evaluate(input, context));
            } else {
                callback(null);
            }
        },
        writer: (output) => {
            // TODO: Types come in, color them appropriately
            return output;
        }
    });
}

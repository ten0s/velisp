const fs = require('fs');
const {Command} = require('commander');
const {GlobalContext} = require('./VeLispContext.js');
const {evaluate} = require('./VeLispEvaluator.js');
const config = require('../package.json');

const program = new Command();
program.version(config.version)
    .arguments('[file]')
    .action((file) => {
        //console.log(file);
        let input = "";
        let stream = null;
        const globalContext = new GlobalContext();
        if (file) {
            //console.log(`Read from ${file}`);
            stream = fs.createReadStream(file);
        } else {
            //console.log('Read from stdin');
            stream = process.stdin;
        }
        if (stream) {
            stream.on('data', (chunk) => {
                input += chunk.toString();
            });
            stream.on('end', () => {
                //console.log(input);
                try {
                    if (input) {
                        evaluate(input, globalContext);
                    }
                } catch (err) {
                    console.error(err.message);
                }
            });
            stream.on('error', console.error);
        }
    })
    .parse(process.argv);

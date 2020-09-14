const fs = require('fs');
const {Command} = require('commander');
const {GlobalContext} = require('./VeLispGlobalContext.js');
const {evaluate} = require('./VeLispEvaluator.js');
const config = require('../package.json');

const program = new Command();
program.version(config.version)
    .arguments('[file]')
    .action((file) => {
        //console.log(file);
        let input = "";
        let stream = null;
        if (file) {
            //console.log(`read from ${file}`);
            stream = fs.createReadStream(file);
        } else {
            //console.log('read from stdin');
            stream = process.stdin;
        }
        stream.on('data', (chunk) => {
            input += chunk.toString();
        });
        stream.on('end', () => {
            //console.log(input);
            evaluate(input, new GlobalContext());
        });
        stream.on('error', console.log);
    })
    .parse(process.argv);

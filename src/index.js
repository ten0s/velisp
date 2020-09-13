const fs = require('fs');
const {Command} = require('commander');
const {evaluate} = require('./VeLispEvaluator.js');
const config = require('../package.json');

const program = new Command();
program.version(config.version)
    .arguments('[file]')
    .action((file) => {
        //console.log(file);
        let buffer = null;
        let stream = null;
        if (file) {
            //console.log(`read from ${file}`);
            stream = fs.createReadStream(file);
        } else {
            //console.log('read from stdin');
            stream = process.stdin;
        }
        stream.on('data', (chunk) => {
            if (!buffer) {
                buffer = chunk;
            } else {
                buffer.push(chunk);
            }
        });
        stream.on('end', () => {
            const input = buffer.toString();
            //console.log(input);
            evaluate(input);
        });
        stream.on('error', console.log);
    })
    .parse(process.argv);

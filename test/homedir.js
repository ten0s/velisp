const os = require('os')
const {TestRunner} = require('./test-runner.js')
const {Str} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'homedir',

    tests: [
        {test: '(homedir)', result: new Str(os.homedir())},
    ],

    errors: [
        {test: '(homedir \'foo)', result: new Error('homedir: too many arguments')},
    ]
})

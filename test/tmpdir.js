const os = require('os')
const {TestRunner} = require('./test-runner.js')
const {Str} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'tmpdir',

    tests: [
        {test: '(tmpdir)', result: new Str(os.tmpdir())},
    ],

    errors: [
        {test: '(tmpdir \'foo)', result: new Error('tmpdir: too many arguments')},
    ]
})

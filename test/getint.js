const {TestRunner} = require('./test-runner.js')

TestRunner.run({
    name: 'getint',

    tests: [
    ],

    errors: [
        {test: '(getint "msg" "msg")', result: new Error('getint: too many arguments')},
        {test: '(getint \'msg)', result: new Error('getint: `msg` expected Str')},
    ]
})

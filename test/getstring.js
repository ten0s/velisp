const {TestRunner} = require('./test-runner.js')

TestRunner.run({
    name: 'getstring',

    tests: [
    ],

    errors: [
        {test: '(getstring \'msg)', result: new Error('getstring: `msg` expected Str')},
        {test: '(getstring \'T "msg")', result: new Error('getstring: `cr` expected Bool')},
        {test: '(getstring T \'msg)', result: new Error('getstring: `msg` expected Str')},
    ]
})

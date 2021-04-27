const {TestRunner} = require('./test-runner.js')

TestRunner.run({
    name: 'getstring',

    tests: [
        // See getstring.{exp,lsp}
    ],

    errors: [
        {test: '(getstring T "msg" "msg")', result: new Error('getstring: too many arguments')},
        {test: '(getstring \'msg)', result: new Error('getstring: `msg` expected Str')},
        {test: '(getstring \'T "msg")', result: new Error('getstring: `cr` expected Bool')},
        {test: '(getstring T \'msg)', result: new Error('getstring: `msg` expected Str')},
    ]
})

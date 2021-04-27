const {TestRunner} = require('./test-runner.js')

TestRunner.run({
    name: 'getreal',

    tests: [
        // See getreal.{exp,lsp}
    ],

    errors: [
        {test: '(getreal "msg" "msg")', result: new Error('getreal: too many arguments')},
        {test: '(getreal \'msg)', result: new Error('getreal: `msg` expected Str')},
    ]
})

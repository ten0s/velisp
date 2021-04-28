const {TestRunner} = require('./test-runner.js')
const {Bool, Int, Str, List} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'prin1',

    tests: [
        {test: '(prin1)', result: new Str('')},
        {test: '(prin1 nil)', result: new Bool(false)},
        {test: '(prin1 1)', result: new Int(1)},
        {test: '(prin1 "msg")', result: new Str('msg')},
        {test: '(prin1 \'(1))', result: new List([new Int(1)])},
        // See prin1.{exp,lsp}
    ],

    errors: [
        {test: '(prin1 "msg" "file" "msg")', result: new Error('prin1: too many arguments')},
        {test: '(prin1 "msg" "file")', result: new Error('prin1: `file-desc` expected File')},
    ]
})

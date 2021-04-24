const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'startapp',

    tests: [
        {test: '(startapp "ls")', result: (act) => Number.isInteger(act.value())},
        {test: '(startapp "ls" "-l")', result: (act) => Number.isInteger(act.value())},
        {test: '(startapp "unknown")', result: new Bool(false)},
    ],

    errors: [
        {test: '(startapp)', result: new Error('startapp: too few arguments')},
        {test: '(startapp "ls" "-l" "")', result: new Error('startapp: too many arguments')},
        {test: '(startapp \'ls)', result: new Error('startapp: `cmd` expected Str')},
        {test: '(startapp "ls" \'opt)', result: new Error('startapp: `file` expected Str')},
    ]
})

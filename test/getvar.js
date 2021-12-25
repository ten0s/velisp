const {TestRunner} = require('./test-runner.js')
const {Bool, Int, Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'getvar',

    tests: [
        {test: '(getvar "cdate")', result: (act) => act instanceof Real},
        {test: '(getvar "CDATE")', result: (act) => act instanceof Real},
        {test: '(getvar \'CDATE)', result: (act) => act instanceof Real},

        {test: '(getvar "millisecs")', result: (act) => act instanceof Int},
        {test: '(getvar "MILLISECS")', result: (act) => act instanceof Int},
        {test: '(getvar \'MILLISECS)', result: (act) => act instanceof Int},

        {test: '(getvar "unknown")', result: new Bool(false)},
        {test: '(getvar "UNKNOWN")', result: new Bool(false)},
        {test: '(getvar \'UNKNOWN)', result: new Bool(false)},
    ],

    errors: [
        {test: '(getvar)', result: new Error('getvar: too few arguments')},
        {test: '(getvar "VAR1" "VAR2")', result: new Error('getvar: too many arguments')},
        {test: '(getvar 1)', result: new Error('getvar: expected Str, Sym')},
    ]
})

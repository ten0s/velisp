const {TestRunner} = require('./test-runner.js')
const {Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'cos',

    tests: [
        {test: '(cos 1)', result: new Real(Math.cos(1))},
        {test: '(cos 1.0)', result: new Real(Math.cos(1.0))},
        {test: '(cos 0)', result: new Real(Math.cos(0))},
        {test: '(cos 0.0)', result: new Real(Math.cos(0.0))},
        {test: '(cos pi)', result: new Real(Math.cos(Math.PI))},
    ],

    errors: [
        {test: '(cos)', result: new Error('cos: too few arguments')},
        {test: '(cos 0 1)', result: new Error('cos: too many arguments')},
    ]
})

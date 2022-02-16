const {TestRunner} = require('./test-runner.js')
const {Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'exp',

    tests: [
        {test: '(exp 1)', result: new Real(Math.exp(1))},
        {test: '(exp 1.0)', result: new Real(Math.exp(1.0))},
        {test: '(exp 2.2)', result: new Real(Math.exp(2.2))},
        {test: '(exp -0.4)', result: new Real(Math.exp(-0.4))},
    ],

    errors: [
        {test: '(exp)', result: new Error('exp: too few arguments')},
        {test: '(exp 0 1)', result: new Error('exp: too many arguments')},
    ]
})

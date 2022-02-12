const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'atan',

    tests: [
        {test: '(atan 1)', result: new Real(Math.atan(1))},
        {test: '(atan 1.0)', result: new Real(Math.atan(1.0))},
        {test: '(atan 0.5)', result: new Real(Math.atan(0.5))},
        {test: '(atan -1.0)', result: new Real(Math.atan(-1.0))},
        {test: '(atan 2.0 3.0)', result: new Real(Math.atan(2.0 / 3.0))},
        {test: '(atan 2.0 -3.0)', result: new Real(Math.atan(2.0 / -3.0))},
        {test: '(atan 1.0 0.0)', result: new Real(Math.PI / 2)},
    ],

    errors: [
        {test: '(atan)', result: new Error('atan: too few arguments')},
        {test: '(atan 2 3 4)', result: new Error('atan: too many arguments')},
        {test: '(atan "2" 3)', result: new Error('atan: expected Int, Real')},
        {test: '(atan 2 "3")', result: new Error('atan: expected Int, Real')},
    ]
})

const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'sqrt',

    tests: [
        {test: '(sqrt 0)', result: new Real(0.0)},
        {test: '(sqrt 4)', result: new Real(2.0)},
        {test: '(sqrt 2.0)', result: new Real(Math.sqrt(2.0))},
    ],

    errors: [
        {test: '(sqrt)', result: new Error('sqrt: too few arguments')},
        {test: '(sqrt -1)', result: new Error('sqrt: expected positive Int, Real')},
        {test: '(sqrt 2 4)', result: new Error('sqrt: too many arguments')},
        {test: '(sqrt "2")', result: new Error('sqrt: expected Int, Real')},
    ]
})

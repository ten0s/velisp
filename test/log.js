const {TestRunner} = require('./test-runner.js')
const {Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'log',

    tests: [
        {test: '(log 4.5)', result: new Real(Math.log(4.5))},
        {test: '(log 1.22)', result: new Real(Math.log(1.22))},
    ],

    errors: [
        {test: '(log)', result: new Error('log: too few arguments')},
        {test: '(log 2 4)', result: new Error('log: too many arguments')},
        {test: '(log "2")', result: new Error('log: expected Int, Real')},
        {test: '(log 0)', result: new Error('log: expected positive Int, Real')},
        {test: '(log -1)', result: new Error('log: expected positive Int, Real')},
    ]
})

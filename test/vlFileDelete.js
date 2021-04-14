const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-file-delete',

    setup: () => {
        evaluate('(close (open "f1" "w"))')
    },

    teardown: () => {
        evaluate('(vl-file-delete "f1")')
    },

    tests: [
        {test: '(vl-file-delete "f1")', result: new Bool(true)},
        {test: '(vl-file-delete "f2")', result: new Bool(false)},
    ],

    errors: [
        {test: '(vl-file-delete)', result: new Error('vl-file-delete: too few arguments')},
        {test: '(vl-file-delete "f1" "f2")', result: new Error('vl-file-delete: too many arguments')},
        {test: '(vl-file-delete \'f1)', result: new Error('vl-file-delete: expected Str')},
    ]
})

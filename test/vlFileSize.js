const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-file-size',

    setup: () => {
        evaluate('(setq f (open "f1" "w")) (princ "Hello" f) (close f)')
        evaluate('(vl-mkdir "d1")')
    },

    teardown: () => {
        evaluate('(vl-file-delete "f1")')
        evaluate('(rmdir "d1")')
    },

    tests: [
        {test: '(vl-file-size "d1")', result: new Int(0)},
        {test: '(vl-file-size "d2")', result: new Bool(false)},
        {test: '(vl-file-size "f1")', result: new Int(5)},
        {test: '(vl-file-size "f2")', result: new Bool(false)},
    ],

    errors: [
        {test: '(vl-file-size)', result: new Error('vl-file-size: too few arguments')},
        {test: '(vl-file-size "f1" "f2")', result: new Error('vl-file-size: too many arguments')},
        {test: '(vl-file-size \'f1)', result: new Error('vl-file-size: expected Str')},
    ]
})

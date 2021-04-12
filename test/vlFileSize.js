const fs = require('fs')
const {TestRunner} = require('./test-runner.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-file-size',

    setup: () => {
        fs.mkdirSync('d1')
    },

    teardown: () => {
        fs.unlinkSync('f1')
        fs.rmdirSync('d1')
    },

    tests: [
        {test: '(vl-file-size "d1")', result: new Int(0)},
        {test: '(vl-file-size "d2")', result: new Bool(false)},
        {test: `(setq f (open "f1" "w"))
            (princ "Hello" f)
            (close f)
            (vl-file-size "f1")`, result: new Int(5)},
    ],

    errors: [
        {test: '(vl-file-size)', result: new Error('vl-file-size: too few arguments')},
        {test: '(vl-file-size "f1" "f2")', result: new Error('vl-file-size: too many arguments')},
        {test: '(vl-file-size \'f1)', result: new Error('vl-file-size: expected Str')},
    ]
})

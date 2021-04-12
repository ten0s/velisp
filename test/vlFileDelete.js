const fs = require('fs')
const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-file-delete',

    setup: () => {},

    teardown: () => {
        fs.unlinkSync('f1')
    },

    tests: [
        {test: '(setq f (open "f1" "w")) (close f) (vl-file-delete "f1")', result: new Bool(true)},
        {test: '(vl-file-delete "f2")', result: new Bool(false)},
    ],

    errors: [
        {test: '(vl-file-delete)', result: new Error('vl-file-delete: too few arguments')},
        {test: '(vl-file-delete "f1" "f2")', result: new Error('vl-file-delete: too many arguments')},
        {test: '(vl-file-delete \'f1)', result: new Error('vl-file-delete: expected Str')},
    ]
})

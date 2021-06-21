const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-file-directory-p',

    setup: () => {
        evaluate('(close (open "f1" "w"))')
        evaluate('(vl-mkdir "d1")')
    },

    teardown: () => {
        evaluate('(vl-file-delete "f1")')
        evaluate('(rmdir "d1")')
    },

    tests: [
        // Existing file
        {test: '(vl-file-directory-p "f1")', result: new Bool(false)},
        // Non-existing file
        {test: '(vl-file-directory-p "f2")', result: new Bool(false)},
        // Existing dir
        {test: '(vl-file-directory-p "d1")', result: new Bool(true)},
        // Non-existing dir
        {test: '(vl-file-directory-p "d2")', result: new Bool(false)},
    ],

    errors: [
        {test: '(vl-file-directory-p)', result: new Error('vl-file-directory-p: too few arguments')},
        {test: '(vl-file-directory-p "f1" "f2")', result: new Error('vl-file-directory-p: too many arguments')},
        {test: '(vl-file-directory-p \'f1)', result: new Error('vl-file-directory-p: expected Str')},
    ]
})

const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-mkdir',

    setup: () => {
        evaluate('(vl-mkdir "d2")')
    },

    teardown: () => {
        evaluate('(rmdir "d1/a/b")')
        evaluate('(rmdir "d1/a")')
        evaluate('(rmdir "d1")')
        evaluate('(rmdir "d2")')
    },

    tests: [
        {test: '(vl-mkdir "d1")', result: new Bool(true)},
        {test: '(vl-mkdir "d2")', result: new Bool(false)},
        {test: '(vl-mkdir "d1/a/b")', result: new Bool(false)},
        {test: '(vl-mkdir "d1") (vl-mkdir "d1/a") (vl-mkdir "d1/a/b")', result: new Bool(true)},
    ],

    errors: [
        {test: '(vl-mkdir)', result: new Error('vl-mkdir: too few arguments')},
        {test: '(vl-mkdir "d1" "d2")', result: new Error('vl-mkdir: too many arguments')},
        {test: '(vl-mkdir \'d1)', result: new Error('vl-mkdir: expected Str')},
    ]
})

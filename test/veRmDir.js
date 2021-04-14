const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 've-rmdir',

    setup: () => {
        evaluate('(vl-mkdir "d1")')
        evaluate('(vl-mkdir "d1/a")')
        evaluate('(vl-mkdir "d1/a/b")')
        evaluate('(vl-mkdir "d2")')
    },

    teardown: () => {
        evaluate('(ve-rmdir "d1/a/b")')
        evaluate('(ve-rmdir "d1/a")')
        evaluate('(ve-rmdir "d1")')
        evaluate('(ve-rmdir "d2")')
    },

    tests: [
        {test: '(ve-rmdir "d1")', result: new Bool(false)},
        {test: '(ve-rmdir "d1/a")', result: new Bool(false)},
        
        {test: '(ve-rmdir "d2")', result: new Bool(true)},
        
        {test: '(ve-rmdir "d1/a/b")', result: new Bool(true)},
        {test: '(ve-rmdir "d1/a/b") (ve-rmdir "d1/a") (ve-rmdir "d1")', result: new Bool(true)},
    ],

    errors: [
        {test: '(ve-rmdir)', result: new Error('ve-rmdir: too few arguments')},
        {test: '(ve-rmdir "d1" "d2")', result: new Error('ve-rmdir: too many arguments')},
        {test: '(ve-rmdir \'d1)', result: new Error('ve-rmdir: expected Str')},
    ]
})

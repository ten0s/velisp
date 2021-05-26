const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'rmdir',

    setup: () => {
        evaluate('(vl-mkdir "d1")')
        evaluate('(vl-mkdir "d1/a")')
        evaluate('(vl-mkdir "d1/a/b")')
        evaluate('(vl-mkdir "d2")')
    },

    teardown: () => {
        evaluate('(rmdir "d1/a/b")')
        evaluate('(rmdir "d1/a")')
        evaluate('(rmdir "d1")')
        evaluate('(rmdir "d2")')
    },

    tests: [
        {test: '(rmdir "d1")', result: new Bool(false)},
        {test: '(rmdir "d1/a")', result: new Bool(false)},
        
        {test: '(rmdir "d2")', result: new Bool(true)},
        
        {test: '(rmdir "d1/a/b")', result: new Bool(true)},
        {test: '(rmdir "d1/a/b") (rmdir "d1/a") (rmdir "d1")', result: new Bool(true)},
    ],

    errors: [
        {test: '(rmdir)', result: new Error('rmdir: too few arguments')},
        {test: '(rmdir "d1" "d2")', result: new Error('rmdir: too many arguments')},
        {test: '(rmdir \'d1)', result: new Error('rmdir: expected Str')},
    ]
})

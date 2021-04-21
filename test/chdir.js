const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'chdir',

    setup: () => {
        evaluate('(vl-mkdir "d1")')
        evaluate('(vl-mkdir "d1/a")')
        evaluate('(vl-mkdir "d1/a/b")')
        return process.cwd()
    },

    teardown: (cwd) => {
        process.chdir(cwd)
        evaluate('(ve-rmdir "d1/a/b")')
        evaluate('(ve-rmdir "d1/a")')
        evaluate('(ve-rmdir "d1")')
    },

    tests: [
        // Existing dir
        {test: '(eq (chdir ".") (cwd))', result: new Bool(true)},
        {test: '(chdir "d1")', result: (act) => act.value().endsWith('d1')},
        {test: '(chdir "d1/a")', result: (act) => act.value().endsWith('d1/a')},
        {test: '(chdir "d1/a/b")', result: (act) => act.value().endsWith('d1/a/b')},
        // Non-existing dir
        {test: '(chdir "d2")', result: new Bool(false)},
    ],

    errors: [
        {test: '(chdir)', result: new Error('chdir: too few arguments')},
        {test: '(chdir "d1" "d2")', result: new Error('chdir: too many arguments')},
        {test: '(chdir \'d1)', result: new Error('chdir: expected Str')},
    ]
})

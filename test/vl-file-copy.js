const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Int, List} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-file-copy',

    setup: () => {
        evaluate('(setq f (open "f1" "w")) (princ "Hello\n" f) (close f)')
        evaluate('(setq f (open "f2" "w")) (princ "Bro\n" f) (close f)')
    },

    teardown: () => {
        evaluate('(vl-file-delete "f1")')
        evaluate('(vl-file-delete "f2")')
        evaluate('(vl-file-delete "f3")')
    },

    tests: [
        // Copy/append to the same file
        {test: '(vl-file-copy "f1" "f1")', result: new Bool(false)},
        {test: '(vl-file-copy "f1" "f1" T)', result: new Bool(false)},

        // Copy to existing
        {test: '(vl-file-copy "f1" "f2")', result: new Bool(false)},
        
        // Copy to non-existing
        {test: '(vl-file-copy "f1" "f3")', result: new Int(6)},

        // Append to non-existing
        {test: '(vl-file-copy "f2" "f3" T)', result: new Int(4)},
        
        // Copy + Append
        {test: `(list
                  (vl-file-copy "f1" "f3")
                  (vl-file-copy "f2" "f3" T)
                  (vl-file-size "f3"))`, result:
         new List([new Int(6), new Int(4), new Int(10)])},
    ],

    errors: [
        {test: '(vl-file-copy)', result: new Error('vl-file-copy: too few arguments')},
        {test: '(vl-file-copy "f1")', result: new Error('vl-file-copy: too few arguments')},
        {test: '(vl-file-copy "f1" "f2" nil nil)', result: new Error('vl-file-copy: too many arguments')},
        {test: '(vl-file-copy \'f1 "f2")', result: new Error('vl-file-copy: `src-filename` expected Str')},
        {test: '(vl-file-copy "f1" \'f2)', result: new Error('vl-file-copy: `dst-filename` expected Str')},
        {test: '(vl-file-copy "f1" "f2" "T")', result: new Error('vl-file-copy: `append` expected Bool')},
    ]
})

import {TestRunner} from './test-runner.js'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Int, List} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-file-rename',

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
        // Rename to the same file
        {test: '(vl-file-rename "f1" "f1")', result: new Bool(false)},

        // Rename to existing
        {test: '(vl-file-rename "f1" "f2")', result: new Bool(false)},

        // Rename to non-existing
        {test: '(vl-file-rename "f1" "f3")', result: new Bool(true)},

        {test: '(list (vl-file-rename "f1" "f3") (vl-file-size "f3"))', result:
         new List([new Bool(true), new Int(6)])},
        {test: '(list (vl-file-rename "f2" "f3") (vl-file-size "f3"))', result:
         new List([new Bool(true), new Int(4)])},
    ],

    errors: [
        {test: '(vl-file-rename)', result: new Error('vl-file-rename: too few arguments')},
        {test: '(vl-file-rename "f1")', result: new Error('vl-file-rename: too few arguments')},
        {test: '(vl-file-rename "f1" "f2" nil)', result: new Error('vl-file-rename: too many arguments')},
        {test: '(vl-file-rename \'f1 "f2")', result: new Error('vl-file-rename: `src-filename` expected Str')},
        {test: '(vl-file-rename "f1" \'f2)', result: new Error('vl-file-rename: `dst-filename` expected Str')},
    ]
})

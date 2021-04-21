const {TestRunner} = require('./test-runner.js')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Str, List} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-directory-files',

    setup: () => {
        evaluate('(vl-mkdir "d1")')
        evaluate('(vl-mkdir "d1/d2")')
        evaluate('(close (open "d1/.f1" "w"))')
        evaluate('(close (open "d1/.f2.conf" "w"))')
        evaluate('(close (open "d1/f3" "w"))')
        evaluate('(close (open "d1/f4.txt" "w"))')
    },

    teardown: () => {
        evaluate('(vl-file-delete "d1/f4.txt")')
        evaluate('(vl-file-delete "d1/f4.txt")')
        evaluate('(vl-file-delete "d1/f3")')
        evaluate('(vl-file-delete "d1/.f2.conf")')
        evaluate('(vl-file-delete "d1/.f1")')
        evaluate('(ve-rmdir "d1/d2")')
        evaluate('(ve-rmdir "d1")')
    },

    tests: [
        // Existing dir
        {test: '(vl-directory-files)', result: (act) => act instanceof List},
        {test: '(vl-directory-files nil)', result: (act) => act instanceof List},
        {test: '(vl-directory-files nil nil)', result: (act) => act instanceof List},
        {test: '(vl-directory-files nil nil nil)', result: (act) => act instanceof List},

        {test: '(vl-directory-files "d1")', result: new List([
            new Str('.'), new Str('..'),
            new Str('.f1'), new Str('.f2.conf'),
            new Str('f4.txt'),
        ])},

        {test: '(vl-directory-files "d1" "*.*")', result: new List([
            new Str('.'), new Str('..'),
            new Str('.f1'), new Str('.f2.conf'),
            new Str('f4.txt'),
        ])},

        {test: '(vl-directory-files "d1" "*.txt")', result: new List([
            new Str('f4.txt'),
        ])},

        {test: '(vl-directory-files "d1" "*")', result: new List([
            new Str('.'), new Str('..'),
            new Str('.f1'), new Str('.f2.conf'),
            new Str('d2'),
            new Str('f3'),
            new Str('f4.txt'),
        ])},

        {test: '(vl-directory-files "d1" "??")', result: new List([
            new Str('..'),
            new Str('d2'),
            new Str('f3'),
        ])},

        {test: '(vl-directory-files "d1" "*" -1)', result: new List([
            new Str('.'), new Str('..'),
            new Str('d2'),
        ])},

        {test: '(vl-directory-files "d1" "*" -1)', result: new List([
            new Str('.'), new Str('..'),
            new Str('d2'),
        ])},

        {test: '(vl-directory-files "d1" "*" 0)', result: new List([
            new Str('.'), new Str('..'),
            new Str('.f1'), new Str('.f2.conf'),
            new Str('d2'),
            new Str('f3'),
            new Str('f4.txt'),
        ])},

        {test: '(vl-directory-files "d1" "*" 1)', result: new List([
            new Str('.f1'), new Str('.f2.conf'),
            new Str('f3'),
            new Str('f4.txt'),
        ])},

        // Non-existing dir
        {test: '(vl-directory-files "d2")', result: new Bool(false)},
    ],

    errors: [
        {test: '(vl-directory-files "d1" "" 0 "")', result: new Error('vl-directory-files: too many arguments')},
        {test: '(vl-directory-files \'d1)', result: new Error('vl-directory-files: `directory` expected Str, Bool')},
        {test: '(vl-directory-files "d1" \'p)', result: new Error('vl-directory-files: `pattern` expected Str, Bool')},
        {test: '(vl-directory-files "d1" "" "0")', result: new Error('vl-directory-files: `what` expected Int, Bool')},
    ]
})

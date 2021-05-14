const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'wcmatch',

    tests: [
        {test: '(wcmatch "" "")', result: new Bool(true)},

        {test: '(wcmatch "0123456789" "##########")', result: new Bool(true)},
        {test: '(wcmatch "azAZ" "@@@@")', result: new Bool(true)},
        {test: '(wcmatch ".,:;" "....")', result: new Bool(true)},

        {test: '(wcmatch "abcd" "*`,*")', result: new Bool(false)},
        {test: '(wcmatch "ab,cd" "*`,*")', result: new Bool(true)},

        {test: '(wcmatch "abcd" "*`\\\\*")', result: new Bool(false)},
        {test: '(wcmatch "ab\\\\cd" "*`\\\\*")', result: new Bool(true)},

        {test: '(wcmatch "abc" "abc" nil)', result: new Bool(true)},
        {test: '(wcmatch "abc" "abc" T)', result: new Bool(true)},
        {test: '(wcmatch "abc" "abc" \'regex)', result: new Bool(true)},
        {test: '(wcmatch "abc" "abc" \'dot)', result: new Bool(true)},
    ],

    errors: [
        {test: '(wcmatch)', result: new Error('wcmatch: too few arguments')},
        {test: '(wcmatch "str" "pat" nil "other")', result: new Error('wcmatch: too many arguments')},
        {test: '(wcmatch  \'str "pat")', result: new Error('wcmatch: `str` expected Str')},
        {test: '(wcmatch  "str" \'pat)', result: new Error('wcmatch: `pattern` expected Str')},
        {test: '(wcmatch  "str" "pat" "other")', result: new Error('wcmatch: `flag` expected Sym, Bool')},
    ]
})

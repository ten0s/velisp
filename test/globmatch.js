const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'globmatch',

    tests: [
        {test: '(globmatch "" "")', result: new Bool(true)},

        {test: '(globmatch "a" "a")', result: new Bool(true)},
        {test: '(globmatch "abc" "abc")', result: new Bool(true)},
        
        {test: '(globmatch "abc" "a?c")', result: new Bool(true)},
        {test: '(globmatch "a_c" "a?c")', result: new Bool(true)},
        
        {test: '(globmatch "abc.txt" "abc.txt")', result: new Bool(true)},
        {test: '(globmatch "abc_txt" "abc.txt")', result: new Bool(false)},
        
        {test: '(globmatch "abc" "*")', result: new Bool(true)},
        {test: '(globmatch "abc.txt" "*.txt")', result: new Bool(true)},
        {test: '(globmatch "abc.txt" "???.txt")', result: new Bool(true)},
        
        {test: '(globmatch "abc" "abc" nil)', result: new Bool(true)},
        {test: '(globmatch "abc" "abc" T)', result: new Bool(true)},
        {test: '(globmatch "abc" "abc" \'regex)', result: new Bool(true)},
        {test: '(globmatch "abc" "abc" \'dot)', result: new Bool(true)},
    ],

    errors: [
        {test: '(globmatch)', result: new Error('globmatch: too few arguments')},
        {test: '(globmatch "str" "pat" nil "other")', result: new Error('globmatch: too many arguments')},
        {test: '(globmatch  \'str "pat")', result: new Error('globmatch: `str` expected Str')},
        {test: '(globmatch  "str" \'pat)', result: new Error('globmatch: `pattern` expected Str')},
        {test: '(globmatch  "str" "pat" "other")', result: new Error('globmatch: `flag` expected Sym, Bool')},
    ]
})

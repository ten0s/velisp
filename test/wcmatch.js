const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'wcmatch',

    tests: [
        {test: '(wcmatch "" "")', result: new Bool(true)},
        {test: '(wcmatch "0123456789" "##########")', result: new Bool(true)},
        {test: '(wcmatch "Name" "*`,*")', result: new Bool(false)},  // is , inside?
        //{test: '(wcmatch "Name" "*`\\*")', result: new Bool(false)}, // is \ inside?
    ],

    errors: [
        {test: '(wcmatch)', result: new Error('wcmatch: too few arguments')},
        {test: '(wcmatch "str" "pat" "other")', result: new Error('wcmatch: too many arguments')},
        {test: '(wcmatch  \'str "pat")', result: new Error('wcmatch: `str` expected Str')},
        {test: '(wcmatch  "str" \'pat)', result: new Error('wcmatch: `pattern` expected Str')},
    ]
})

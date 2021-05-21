const {TestRunner} = require('./test-runner.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-string-mismatch',

    tests: [
        {test: '(vl-string-mismatch "VL-FUN" "VL-VAR")', result: new Int(3)},

        {test: '(vl-string-mismatch "vl-fun" "avl-var")', result: new Int(0)},
        {test: '(vl-string-mismatch "vl-fun" "avl-var" 0 1)', result: new Int(3)},

        {test: '(vl-string-mismatch "VL-FUN" "Vl-vAR")', result: new Int(1)},
        {test: '(vl-string-mismatch "VL-FUN" "Vl-vAR" 0 0 T)', result: new Int(3)},
    ],

    errors: [
        {test: '(vl-string-mismatch)', result:
         new Error('vl-string-mismatch: too few arguments')},
        {test: '(vl-string-mismatch "bar")', result:
         new Error('vl-string-mismatch: too few arguments')},

        {test: '(vl-string-mismatch "bar" "baz" 0 0 nil nil)', result:
         new Error('vl-string-mismatch: too many arguments')},

        {test: '(vl-string-mismatch \'bar "baz")', result:
         new Error('vl-string-mismatch: `str1` expected Str')},

        {test: '(vl-string-mismatch "bar" \'baz)', result:
         new Error('vl-string-mismatch: `str2` expected Str')},
        
        {test: '(vl-string-mismatch "bar" "baz" "0")', result:
         new Error('vl-string-mismatch: `pos1` expected Int')},
        {test: '(vl-string-mismatch "bar" "baz" -1)', result:
         new Error('vl-string-mismatch: `pos1` expected non-negative Int')},

        {test: '(vl-string-mismatch "bar" "baz" 0 "0")', result:
         new Error('vl-string-mismatch: `pos2` expected Int')},
        {test: '(vl-string-mismatch "bar" "baz" 0 -1)', result:
         new Error('vl-string-mismatch: `pos2` expected non-negative Int')},

        {test: '(vl-string-mismatch "bar" "baz" 0 0 "true")', result:
         new Error('vl-string-mismatch: `ignore-case-p` expected Bool')},
    ]
})

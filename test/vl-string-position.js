const {TestRunner} = require('./test-runner.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-string-position',

    tests: [
        {test: '(vl-string-position (ascii "z") "azbdc")', result: new Int(1)},
        {test: '(vl-string-position 122 "azbzc")', result: new Int(1)},
        {test: '(vl-string-position (ascii "x") "azbzc")', result: new Bool(false)},

        {test: '(vl-string-position (ascii "z") "azbzc")', result: new Int(1)},
        {test: '(vl-string-position (ascii "z") "azbzc" 0)', result: new Int(1)},
        {test: '(vl-string-position (ascii "z") "azbzc" nil)', result: new Int(1)},
        {test: '(vl-string-position (ascii "z") "azbzc" 2)', result: new Int(3)},
        
        {test: '(vl-string-position (ascii "z") "azbzlmnqc" nil T)', result: new Int(3)},
    ],

    errors: [
        {test: '(vl-string-position)', result:
         new Error('vl-string-position: too few arguments')},
        {test: '(vl-string-position 98)', result:
         new Error('vl-string-position: too few arguments')},

        {test: '(vl-string-position 98 "bar" 0 nil nil)', result:
         new Error('vl-string-position: too many arguments')},

        {test: '(vl-string-position "b" "bar")', result:
         new Error('vl-string-position: `char-code` expected Int')},
        
        {test: '(vl-string-position 98 nil)', result:
         new Error('vl-string-position: `string` expected Str')},

        {test: '(vl-string-position 98 "bar" "1")', result:
         new Error('vl-string-position: `start-pos` expected Int, Bool')},
        {test: '(vl-string-position 98 "bar" -1)', result:
         new Error('vl-string-position: `start-pos` expected non-negative Int')},
        {test: '(vl-string-position 98 "bar" "1" nil)', result:
         new Error('vl-string-position: `start-pos` expected Int, Bool')},
        {test: '(vl-string-position 98 "bar" -1 nil)', result:
         new Error('vl-string-position: `start-pos` expected non-negative Int')},

        {test: '(vl-string-position 98 "bar" nil 0)', result:
         new Error('vl-string-position: `from-end-p` expected Bool')},
    ]
})

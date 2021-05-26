const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'boundp',

    tests: [
        {test: '(boundp T)', result: new Bool(true)},
        {test: '(boundp \'T)', result: new Bool(true)},
        {test: '(boundp \'PI)', result: new Bool(true)},
        {test: '(boundp \'boundp)', result: new Bool(true)},
        {test: '(boundp \'+)', result: new Bool(true)},
        
        {test: '(boundp \'nil)', result: new Bool(false)},
        {test: '(boundp nil)', result: new Bool(false)},
        {test: '(boundp ())', result: new Bool(false)},
        {test: '(boundp \'())', result: new Bool(false)},
        {test: '(boundp (list))', result: new Bool(false)},
        {test: '(boundp (list 1 2 3))', result: new Bool(false)},
        {test: '(boundp "1 2 3")', result: new Bool(false)},
        {test: '(boundp 1)', result: new Bool(false)},
        {test: '(boundp 1.0)', result: new Bool(false)},
        
        {test: '(boundp \'unk1)', result: new Bool(false)},
        {test: '(setq unk2 T) (boundp \'unk2)', result: new Bool(true)},
    ],

    errors: [
        {test: '(boundp)', result: new Error('boundp: too few arguments')},
        {test: '(boundp \'a \'b)', result: new Error('boundp: too many arguments')},
    ]
})

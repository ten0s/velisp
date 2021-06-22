const {TestRunner} = require('./test-runner.js')
const {Bool, Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-symbol-value',

    tests: [
        {test: '(vl-symbol-value t)', result: new Bool(true)},
        {test: '(vl-symbol-value \'t)', result: new Bool(true)},
        {test: '(vl-symbol-value \'PI)', result: new Real(Math.PI)},
    ],

    errors: [
        {test: '(vl-symbol-value)', result: new Error('vl-symbol-value: too few arguments')},
        {test: '(vl-symbol-value \'foo \'bar)', result: new Error('vl-symbol-value: too many arguments')},
        {test: '(vl-symbol-value nil)', result: new Error('vl-symbol-value: expected Sym')},
        {test: '(vl-symbol-value \'nil)', result: new Error('vl-symbol-value: expected Sym')},
        {test: '(vl-symbol-value "foo")', result: new Error('vl-symbol-value: expected Sym')},
    ]
})

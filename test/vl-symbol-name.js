const {TestRunner} = require('./test-runner.js')
const {Str} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-symbol-name',

    tests: [
        {test: '(vl-symbol-name t)', result: new Str('T')},
        {test: '(vl-symbol-name \'t)', result: new Str('T')},
        {test: '(vl-symbol-name \'foo)', result: new Str('FOO')},
        {test: '(vl-symbol-name \'FOO)', result: new Str('FOO')},
    ],

    errors: [
        {test: '(vl-symbol-name)', result: new Error('vl-symbol-name: too few arguments')},
        {test: '(vl-symbol-name \'foo \'bar)', result: new Error('vl-symbol-name: too many arguments')},
        {test: '(vl-symbol-name \'nil)', result: new Error('vl-symbol-name: expected Sym')},
        {test: '(vl-symbol-name "foo")', result: new Error('vl-symbol-name: expected Sym')},
    ]
})

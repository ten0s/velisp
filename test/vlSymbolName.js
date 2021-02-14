const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Str} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(vl-symbol-name \'t)', result: new Str('T')},
    {test: '(vl-symbol-name \'foo)', result: new Str('FOO')},
    {test: '(vl-symbol-name \'FOO)', result: new Str('FOO')},
]

const errors = [
    {test: '(vl-symbol-name)', result: new Error('vl-symbol-name: too few arguments')},
    {test: '(vl-symbol-name \'foo \'bar)', result: new Error('vl-symbol-name: too many arguments')},
    {test: '(vl-symbol-name \'nil)', result: new Error('vl-symbol-name: expected Sym')},
    {test: '(vl-symbol-name "foo")', result: new Error('vl-symbol-name: expected Sym')},
]
    
QUnit.test('vl-symbol-name', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

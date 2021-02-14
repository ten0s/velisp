const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Real} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(cos 1)', result: new Real(Math.cos(1))},
    {test: '(cos 1.0)', result: new Real(Math.cos(1.0))},
    {test: '(cos 0)', result: new Real(Math.cos(0))},
    {test: '(cos 0.0)', result: new Real(Math.cos(0.0))},
    {test: '(cos pi)', result: new Real(Math.cos(Math.PI))},
]

const errors = [
    {test: '(cos)', result: new Error('cos: too few arguments')},
    {test: '(cos 0 1)', result: new Error('cos: too many arguments')},
]

QUnit.test('cos', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

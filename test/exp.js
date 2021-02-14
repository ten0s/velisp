const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Real} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(exp 1)', result: new Real(Math.exp(1))},
    {test: '(exp 1.0)', result: new Real(Math.exp(1.0))},
    {test: '(exp 2.2)', result: new Real(Math.exp(2.2))},
    {test: '(exp -0.4)', result: new Real(Math.exp(-0.4))},
]

const errors = [
    {test: '(exp)', result: new Error('exp: too few arguments')},
    {test: '(exp 0 1)', result: new Error('exp: too many arguments')},
]

QUnit.test('exp', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

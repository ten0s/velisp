const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Real} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(sqrt 4)', result: new Real(2.0)},
    {test: '(sqrt 2.0)', result: new Real(Math.sqrt(2.0))},
]

const errors = [
    {test: '(sqrt)', result: new Error('sqrt: too few arguments')},
    {test: '(sqrt 2 4)', result: new Error('sqrt: too many arguments')},
    {test: '(sqrt "2")', result: new Error('sqrt: expected Int, Real')},
]

QUnit.test('sqrt', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

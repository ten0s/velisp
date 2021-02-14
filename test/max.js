const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Int, Real} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(max)', result: new Int(0)},
    {test: '(max 1)', result: new Int(1)},
    {test: '(max 1.0)', result: new Real(1.0)},
    {test: '(max 1 1.0)', result: new Real(1.0)},
    {test: '(max 683 -10.0)', result: new Real(683.0)},
    {test: '(max 73 2 48 5)', result: new Int(73)},
    {test: '(max 73 2.0 48 5)', result: new Real(73.0)},
    {test: '(max 2 4 6.7)', result: new Real(6.7)},
]

const errors = [
    {test: '(max "1")', result: new Error('max: expected Int, Real')},
    {test: '(max 1 "2")', result: new Error('max: expected Int, Real')},
    {test: '(max 1 "2" 3)', result: new Error('max: expected Int, Real')},
]

QUnit.test('max', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

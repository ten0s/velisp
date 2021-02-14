const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Str} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(itoa 33)', result: new Str('33')},
    {test: '(itoa -17)', result: new Str('-17')},
]

const errors = [
    {test: '(itoa)', result: new Error('itoa: too few arguments')},
    {test: '(itoa 1 2)', result: new Error('itoa: too many arguments')},
    {test: '(itoa "1")', result: new Error('itoa: expected Int')},
]

QUnit.test('itoa', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

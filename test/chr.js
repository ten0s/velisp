const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Str} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(chr 65)', result: new Str('A')},
    {test: '(chr 66)', result: new Str('B')},
    {test: '(chr 97)', result: new Str('a')},
    // TODO: (chr 10) (chr 13) (chr 9)
]

const errors = [
    {test: '(chr)', result: new Error('chr: too few arguments')},
    {test: '(chr 65 66)', result: new Error('chr: too many arguments')},
    {test: '(chr 66.0)', result: new Error('chr: expected Int')},
]

QUnit.test('chr', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

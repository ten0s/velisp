const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Int} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(ascii "A")', result: new Int(65)},
    {test: '(ascii "a")', result: new Int(97)},
    {test: '(ascii "BIG")', result: new Int(66)},
    // TODO (ascii "\n") (ascii "\r") (ascii "\t")
]

const errors = [
    {test: '(ascii)', result: new Error('ascii: too few arguments')},
    {test: '(ascii "A" "B")', result: new Error('ascii: too many arguments')},
    {test: '(ascii \'a)', result: new Error('ascii: expected Str')},
    {test: '(ascii "")', result: new Error('ascii: expected non-empty Str')},
]

QUnit.test('ascii', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

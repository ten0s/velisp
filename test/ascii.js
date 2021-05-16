const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Int} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(ascii "")', result: new Int(0)},
    {test: '(ascii "A")', result: new Int(65)},
    {test: '(ascii "a")', result: new Int(97)},
    {test: '(ascii "BIG")', result: new Int(66)},
    {test: '(ascii "\\r")', result: new Int(13)},
    {test: '(ascii "\\n")', result: new Int(10)},
    {test: '(ascii "\\t")', result: new Int(9)},
    {test: '(ascii "\\e")', result: new Int(27)},
]

const errors = [
    {test: '(ascii)', result: new Error('ascii: too few arguments')},
    {test: '(ascii "A" "B")', result: new Error('ascii: too many arguments')},
    {test: '(ascii \'a)', result: new Error('ascii: expected Str')},
]

QUnit.test('ascii', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

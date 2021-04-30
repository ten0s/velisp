const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Str} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(substr "abcde" 1)', result: new Str('abcde')},
    {test: '(substr "abcde" 2)', result: new Str('bcde')},
    {test: '(substr "abcde" 5)', result: new Str('e')},
    {test: '(substr "abcde" 10)', result: new Str('')},

    {test: '(substr "abcde" 1 0)', result: new Str('')},
    {test: '(substr "abcde" 1 1)', result: new Str('a')},
    {test: '(substr "abcde" 1 2)', result: new Str('ab')},
    {test: '(substr "abcde" 1 10)', result: new Str('abcde')},

    {test: '(substr "abcde" 2 0)', result: new Str('')},
    {test: '(substr "abcde" 2 1)', result: new Str('b')},

    {test: '(substr "abcde" 3 2)', result: new Str('cd')},

    {test: '(substr "\\r\\n\\t\\e" 1 1)', result: new Str('\r')},
    {test: '(substr "\\r\\n\\t\\e" 2 1)', result: new Str('\n')},
    {test: '(substr "\\r\\n\\t\\e" 4 1)', result: new Str('\u001b')},
]

const errors = [
    {test: '(substr)', result: new Error('substr: too few arguments')},
    {test: '(substr "abcde")', result: new Error('substr: too few arguments')},
    {test: '(substr "abcde" 1 3 4)', result: new Error('substr: too many arguments')},
    {test: '(substr \'abcde 1)', result: new Error('substr: `string` expected Str')},
    {test: '(substr "abcde" "1")', result: new Error('substr: `start` expected Int')},
    {test: '(substr "abcde" 0)', result: new Error('substr: `start` expected positive Int')},
    {test: '(substr "abcde" 1 "3")', result: new Error('substr: `length` expected Int')},
    {test: '(substr "abcde" 1 -1)', result: new Error('substr: `length` expected non-negative Int')},
]

QUnit.test('substr', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

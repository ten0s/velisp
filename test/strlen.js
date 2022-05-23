import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Int} from '../src/VeLispTypes.js'

const tests = [
    {test: '(strlen)', result: new Int(0)},
    {test: '(strlen "")', result: new Int(0)},
    {test: '(strlen "" "")', result: new Int(0)},
    {test: '(strlen "abcd")', result: new Int(4)},
    {test: '(strlen "one" "two" "four")', result: new Int(10)},
    {test: '(strlen "abc\\0def")', result: new Int(3)},
]

const errors = [
    {test: '(strlen \'one)', result: new Error('strlen: expected Str')},
    {test: '(strlen "one" \'two)', result: new Error('strlen: expected Str')},
    {test: '(strlen "one" "two" \'four)', result: new Error('strlen: expected Str')},
    {test: '(strlen "one" \'two "four")', result: new Error('strlen: expected Str')},
]

QUnit.test('strlen', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Int, Real} from '../src/VeLispTypes.js'

const tests = [
    {test: '(rem)', result: new Int(0)},
    {test: '(rem 11)', result: new Int(11)},
    {test: '(rem 11.0)', result: new Real(11.0)},
    {test: '(rem 42 12)', result: new Int(6)},
    {test: '(rem 12.0 16)', result: new Real(12.0)},
    {test: '(rem 26 7 2)', result: new Int(1)},
    {test: '(rem 26 7 2.0)', result: new Real(1.0)},
]

const errors = [
    {test: '(rem "1")', result: new Error('rem: expected Int, Real')},
    {test: '(rem 1 "2")', result: new Error('rem: expected Int, Real')},
    {test: '(rem 1 "2" 3)', result: new Error('rem: expected Int, Real')},
]

QUnit.test('rem', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

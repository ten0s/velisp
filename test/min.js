import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Int, Real} from '../src/VeLispTypes.js'

const tests = [
    {test: '(min)', result: new Int(0)},
    {test: '(min 1)', result: new Int(1)},
    {test: '(min 1.0)', result: new Real(1.0)},
    {test: '(min 1 1.0)', result: new Real(1.0)},
    {test: '(min 683 -10.0)', result: new Real(-10.0)},
    {test: '(min 73 2 48 5)', result: new Int(2)},
    {test: '(min 73.0 2 48 5)', result: new Real(2.0)},
    {test: '(min 2 4 6.7)', result: new Real(2.0)},
]

const errors = [
    {test: '(min "1")', result: new Error('min: expected Int, Real')},
    {test: '(min 1 "2")', result: new Error('min: expected Int, Real')},
    {test: '(min 1 "2" 3)', result: new Error('min: expected Int, Real')},
]

QUnit.test('min', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

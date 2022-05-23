import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Int} from '../src/VeLispTypes.js'

const tests = [
    {test: '(~ 0)', result: new Int(-1)},
    {test: '(~ -1)', result: new Int(0)},
    {test: '(~ 3)', result: new Int(-4)},
    {test: '(~ -4)', result: new Int(3)},
    {test: '(~ 100)', result: new Int(-101)},
    {test: '(~ -101)', result: new Int(100)},
]

const errors = [
    {test: '(~)', result: new Error('~: too few arguments')},
    {test: '(~ 1 2)', result: new Error('~: too many arguments')},
    {test: '(~ 0.0)', result: new Error('~: expected Int')},
    {test: '(~ "0")', result: new Error('~: expected Int')},
    {test: '(~ \'one)', result: new Error('~: expected Int')},
]

QUnit.test('bitwiseNot', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

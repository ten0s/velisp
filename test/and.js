import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool} from '../src/VeLispTypes.js'

const tests = [
    {test: '(and)', result: new Bool(true)},
    {test: '(And)', result: new Bool(true)},
    {test: '(ANd)', result: new Bool(true)},
    {test: '(AND)', result: new Bool(true)},

    {test: '(and nil)', result: new Bool(false)},
    {test: '(and T)', result: new Bool(true)},

    {test: '(and nil nil)', result: new Bool(false)},
    {test: '(and T T)', result: new Bool(true)},
    {test: '(and T nil)', result: new Bool(false)},
    {test: '(and nil T)', result: new Bool(false)},

    {test: '(and T nil nil)', result: new Bool(false)},
    {test: '(and nil T nil)', result: new Bool(false)},
    {test: '(and nil nil T)', result: new Bool(false)},

    {test: '(and T T T)', result: new Bool(true)},
]

QUnit.test('and', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

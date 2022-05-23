import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Str} from '../src/VeLispTypes.js'

const tests = [
    {test: '(cwd)', result: new Str(process.cwd())},
]

const errors = [
    {test: '(cwd \'foo)', result: new Error('cwd: too many arguments')},
]

QUnit.test('cwd', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

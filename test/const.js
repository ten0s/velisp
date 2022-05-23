import QUnit from 'qunit'
import {EOL} from 'os'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Real, Str} from '../src/VeLispTypes.js'

const tests = [
    {test: 'T', result: new Bool(true)},
    {test: 'PI', result: new Real(Math.PI)},
    {test: 'EOL', result: new Str(EOL)},
]

QUnit.test('const', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Str} from '../src/VeLispTypes.js'

const tests = [
    {test: '(chr 0)', result: new Str('')},
    {test: '(chr 65)', result: new Str('A')},
    {test: '(chr 66)', result: new Str('B')},
    {test: '(chr 97)', result: new Str('a')},
    {test: '(chr 13)', result: new Str('\r')},
    {test: '(chr 10)', result: new Str('\n')},
    {test: '(chr 9)', result: new Str('\t')},
    {test: '(chr 27)', result: new Str('\u001b')},
]

const errors = [
    {test: '(chr)', result: new Error('chr: too few arguments')},
    {test: '(chr 65 66)', result: new Error('chr: too many arguments')},
    {test: '(chr 66.0)', result: new Error('chr: expected Int')},
]

QUnit.test('chr', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

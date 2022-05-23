import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Str} from '../src/VeLispTypes.js'

const tests = [
    {test: '(strcat)', result: new Str('')},
    {test: '(strcat "a" "bout")', result: new Str('about')},
    {test: '(strcat "a" "b" "c")', result: new Str('abc')},
    {test: '(strcat "a" "" "c")', result: new Str('ac')},
]

const errors = [
    {test: '(strcat 1)', result: new Error('strcat: expected Str')},
    {test: '(strcat "1" 2)', result: new Error('strcat: expected Str')},
    {test: '(strcat "1" "2" 3)', result: new Error('strcat: expected Str')},
    {test: '(strcat "1" 2 "3")', result: new Error('strcat: expected Str')},
]

QUnit.test('strcat', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

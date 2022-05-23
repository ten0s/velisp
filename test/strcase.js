import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Str} from '../src/VeLispTypes.js'

const tests = [
    {test: '(strcase "Sample")', result: new Str('SAMPLE')},
    {test: '(strcase "Sample" nil)', result: new Str('SAMPLE')},
    {test: '(strcase "Sample" (list))', result: new Str('SAMPLE')},

    {test: '(strcase "Sample" T)', result: new Str('sample')},
    {test: '(strcase "Sample" 1)', result: new Str('sample')},
    {test: '(strcase "Sample" (list 1))', result: new Str('sample')},
]

const errors = [
    {test: '(strcase)', result: new Error('strcase: too few arguments')},
    {test: '(strcase "one" \'nil "two")', result: new Error('strcase: too many arguments')},
    {test: '(strcase \'one)', result: new Error('strcase: expected Str')},
]

QUnit.test('strcase', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

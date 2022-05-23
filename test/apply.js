import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Int, Str, List} from '../src/VeLispTypes.js'

const tests = [
    {test: '(apply (defun foo () "foo") (list))', result: new Str('foo')},
    {test: '(apply (defun foo () "foo") nil)', result: new Str('foo')},
    {test: '(apply (lambda () "foo") (list))', result: new Str('foo')},
    {test: '(apply \'(lambda () "foo") (list))', result: new Str('foo')},
    {test: '(apply (lambda (x) (* x x)) (list 3))', result: new Int(9)},
    {test: '(apply \'(lambda (x) (* x x)) (list 3))', result: new Int(9)},
    {test: '(apply \'+ (list 1 2 3))', result: new Int(6)},
    {test: '(apply \'append \'((97) (96 44) (98) (96 44) (99)))', result: new List([
        new Int(97), new Int(96), new Int(44), new Int(98),
        new Int(96), new Int(44), new Int(99)
    ])},
]

const errors = [
    {test: '(apply)', result: new Error('apply: too few arguments')},
    {test: '(apply \'+)', result: new Error('apply: too few arguments')},
    {test: '(apply \'+ (list 1 2) (list 3 4))', result: new Error('apply: too many arguments')},
    {test: '(apply \'+ 1)', result: new Error('apply: `list` expected List')},
    {test: '(apply \'add (list 1 2 3))', result: new Error('apply: no such function ADD')},
]

QUnit.test('apply', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Int, Real, Str, List, Pair} from '../src/VeLispTypes.js'

const tests = [
    {test: '(list)', result: new List([])},
    {test: '(list 1 2 3)', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
    {test: '(list 1 "2" 3.0 (list 4))', result: new List([
        new Int(1), new Str('2'), new Real(3.0), new List([new Int(4)])
    ])},

    {test: '(cons 1 (list))', result: new List([new Int(1)])},
    {test: '(cons 1 nil)', result: new List([new Int(1)])},
    {test: '(cons 1 (list 2 3))', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
    {test: '(cons 1 (cons 2 (cons 3 nil)))', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
    {test: '(cons (list 1) (list 2 3))', result: new List([
        new List([new Int(1)]), new Int(2), new Int(3)
    ])},

    {test: '(cons 1 (cons 2 3))', result: new List([
        new Int(1), new Pair(new Int(2), new Int(3))
    ])},

    {test: '(car (list 1 2 3))', result: new Int(1)},
    {test: '(car (cons 1 2))', result: new Int(1)},
    {test: '(car (list))', result: new Bool(false)},
    {test: '(car nil)', result: new Bool(false)},
    {test: '(car ())', result: new Bool(false)},

    {test: '(cdr (list 1 2 3))', result: new List([
        new Int(2), new Int(3)
    ])},
    {test: '(cdr (cons 1 2))', result: new Int(2)},
    {test: '(cdr (list))', result: new Bool(false)},
    {test: '(cdr nil)', result: new Bool(false)},
    {test: '(cdr ())', result: new Bool(false)},
]

const errors = [
    {test: '(cons)', result: new Error('cons: too few arguments')},
    {test: '(cons 1)', result: new Error('cons: too few arguments')},
    {test: '(cons 1 2 3)', result: new Error('cons: too many arguments')},

    {test: '(car)', result: new Error('car: too few arguments')},
    {test: '(car (list 1 2) (list 3))', result: new Error('car: too many arguments')},
    {test: '(car T)', result: new Error('car: expected List, Pair')},

    {test: '(cdr)', result: new Error('cdr: too few arguments')},
    {test: '(cdr (list 1 2) (list 3))', result: new Error('cdr: too many arguments')},
    {test: '(cdr T)', result: new Error('cdr: expected List, Pair')},
]

QUnit.test('list', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

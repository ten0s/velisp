import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Int, Real, Str, Sym, List, Pair} from '../src/VeLispTypes.js'

const tests = [
    {test: '(quote nil)', result: new Bool(false)},
    {test: '\'nil', result: new Bool(false)},

    {test: '(quote T)', result: new Sym('t')},
    {test: '\'T', result: new Sym('t')},

    {test: '(quote 1)', result: new Int(1)},
    {test: '\'1', result: new Int(1)},

    {test: '(quote 1.0)', result: new Real(1.0)},
    {test: '\'1.0', result: new Real(1.0)},

    {test: '(quote "1")', result: new Str('1')},
    {test: '\'"1"', result: new Str('1')},

    {test: '(quote foo)', result: new Sym('foo')},
    {test: '\'foo', result: new Sym('foo')},

    {test: '(quote ())', result: new List([])},
    {test: '\'()', result: new List([])},

    {test: '(quote (1))', result: new List([new Int(1)])},
    {test: '\'(1)', result: new List([new Int(1)])},

    {test: '(quote (nil T 1 1.0 "1" foo))', result: new List([
        new Bool(false), new Sym('t'), new Int(1), new Real(1.0), new Str('1'), new Sym('foo')
    ])},
    {test: '\'(nil T 1 1.0 "1" foo)', result: new List([
        new Bool(false), new Sym('t'), new Int(1), new Real(1.0), new Str('1'), new Sym('foo')
    ])},

    {test: '(quote (list))', result: new List([new Sym('list')])},
    {test: '\'(list)', result: new List([new Sym('list')])},

    {test: '(quote (list 1 2 3))', result: new List([
        new Sym('list'), new Int(1), new Int(2), new Int(3)
    ])},
    {test: '\'(list 1 2 3)', result: new List([
        new Sym('list'), new Int(1), new Int(2), new Int(3)
    ])},

    {test: '(quote (1 (2) ((3))))', result: new List([
        new Int(1),
        new List([new Int(2)]),
        new List([new List([new Int(3)])])
    ])},

    {test: '(quote (a . b))', result: new Pair(new Sym('a'), new Sym('b'))},
    {test: '\'(a . b))', result: new Pair(new Sym('a'), new Sym('b'))},

    {test: '(quote (1 2 . z))', result: new List([
        new Int(1), new Pair(new Int(2), new Sym('z'))
    ])},
    {test: '\'(1 2 . z)', result: new List([
        new Int(1), new Pair(new Int(2), new Sym('z'))
    ])},

    {test: '(apply (quote (lambda (x) x)) (quote (z)))', result: new Sym('z')},
    {test: '(apply \'(lambda (x) x) \'(z))', result: new Sym('z')},

    {test: '(quote (quote foo))', result: new List([new Sym('quote'), new Sym('foo')])},
    {test: '\'(quote foo)', result: new List([new Sym('quote'), new Sym('foo')])},
]

const errors = [
    {test: '\'(and)', result: new Error('quote: `and` not supported')},
    {test: '\'(cond)', result: new Error('quote: `cond` not supported')},
    {test: '\'(defun id (x) x)', result: new Error('quote: `defun` not supported')},
    {test: '\'(foreach x \'())', result: new Error('quote: `foreach` not supported')},
    {test: '\'(if T T)', result: new Error('quote: `if` not supported')},
    {test: '\'(or)', result: new Error('quote: `or` not supported')},
    {test: '\'(progn)', result: new Error('quote: `progn` not supported')},
    {test: '\'(repeat 1)', result: new Error('quote: `repeat` not supported')},
    {test: '\'(setq)', result: new Error('quote: `setq` not supported')},
    {test: '\'(while nil nil)', result: new Error('quote: `while` not supported')},
]

QUnit.test('quote', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

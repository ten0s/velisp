import {TestRunner} from './test-runner.js'
import {Bool, Int, Real, Str, Sym, List, Pair, UFun} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'quote',

    tests: [
        {test: '(quote nil)', result: new Bool(false)},
        {test: '\'nil', result: new Bool(false)},

        {test: '(quote T)', result: new Sym('t')},
        {test: '\'T', result: new Sym('t')},

        {test: '(quote 1)', result: new Int(1)},
        {test: '\'1', result: new Int(1)},

        {test: '(quote 2.0)', result: new Real(2.0)},
        {test: '\'2.0', result: new Real(2.0)},

        {test: '(quote "three")', result: new Str('three')},
        {test: '\'"three"', result: new Str('three')},

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

        {test: '(quote (1 2 . z))', result: new Pair(
            new Int(1), new Pair(new Int(2), new Sym('z'))
        )},
        {test: '\'(1 2 . z)', result: new Pair(
            new Int(1), new Pair(new Int(2), new Sym('z'))
        )},

        {test: '(quote (quote foo))', result: new List([new Sym('quote'), new Sym('foo')])},
        {test: '\'(quote foo)', result: new List([new Sym('quote'), new Sym('foo')])},

        //
        // Special forms
        // See more examples in read.js
        //

        {test: '(quote (and))', result: new List([
            new Sym('and')
        ])},
        {test: '(quote (and T nil))', result: new List([
            new Sym('and'), new Bool(true), new Bool(false)
        ])},

        {test: '(quote (cond))', result: new List([
            new Sym('cond')
        ])},
        {test: '(quote (cond (nil)))', result: new List([
            new Sym('cond'),
            new List([new Bool(false)])
        ])},
        {test: '(quote (cond (T)))', result: new List([
            new Sym('cond'),
            new List([new Bool(true)])
        ])},
        {test: '(quote (cond (nil 0) (T 1)))', result: new List([
            new Sym('cond'),
            new List([new Bool(false), new Int(0)]),
            new List([new Bool(true), new Int(1)]),
        ])},

        {test: '(quote (defun foo () "foo"))', result: new List([
            new Sym('defun'),
            new Sym('foo'),
            new Sym('('),
            new Sym(')'),
            new Str('foo'),
        ])},
        {test: '(quote (defun id (x) x))', result: new List([
            new Sym('defun'),
            new Sym('id'),
            new Sym('('),
            new Sym('x'),
            new Sym(')'),
            new Sym('x'),
        ])},

        {test: '(quote (foreach n ()))', result: new List([
            new Sym('foreach'),
            new Sym('n'),
            new Bool(false)
        ])},
        {test: '(quote (foreach n (list 1 2 3)))', result: new List([
            new Sym('foreach'),
            new Sym('n'),
            new List([new Sym('list'), new Int(1), new Int(2), new Int(3)])
        ])},
        {test: '(quote (foreach n (list 1 2 3) n))', result: new List([
            new Sym('foreach'),
            new Sym('n'),
            new List([new Sym('list'), new Int(1), new Int(2), new Int(3)]),
            new Sym('n')
        ])},

        {test: '(quote (function car))', result: new List([
            new Sym('function'),
            new Sym('car'),
        ])},
        {test: '(quote (function (lambda (x) x)))', result: new List([
            new Sym('function'),
            new List([
                new Sym('lambda'),
                new Sym('('),
                new Sym('x'),
                new Sym(')'),
                new Sym('x')
            ])
        ])},

        {test: '(quote (if T 1 0))', result: new List([
            new Sym('if'),
            new Bool(true),
            new Int(1),
            new Int(0)
        ])},
        {test: '(quote (if T 1))', result: new List([
            new Sym('if'),
            new Bool(true),
            new Int(1)
        ])},

        {test: '(quote (lambda () "foo"))', result: (act) => {
            return act instanceof UFun
        }},
        {test: '(quote (lambda (x) x))', result: (act) => {
            return act instanceof UFun
        }},
        {test: '(apply (quote (lambda (x) x)) (quote (z)))', result: new Sym('z')},
        {test: '(apply \'(lambda (x) x) \'(z))', result: new Sym('z')},

        {test: '(quote (or))', result: new List([
            new Sym('or')
        ])},
        {test: '(quote (or T nil))', result: new List([
            new Sym('or'), new Bool(true), new Bool(false)
        ])},

        {test: '(quote (progn))', result: new List([
            new Sym('progn')
        ])},
        {test: '(quote (progn 1))', result: new List([
            new Sym('progn'),
            new Int(1)
        ])},
        {test: '(quote (progn 1 2 (+ 1 2)))', result: new List([
            new Sym('progn'),
            new Int(1),
            new Int(2),
            new List([new Sym('+'), new Int(1), new Int(2)])
        ])},

        {test: '(quote (quote nil))', result: new List([
            new Sym('quote'),
            new Bool(false)
        ])},
        {test: '\'\'nil', result: new List([
            new Sym('quote'),
            new Bool(false)
        ])},
        {test: '(quote (quote (quote nil)))', result: new List([
            new Sym('quote'),
            new List([
                new Sym('quote'),
                new Bool(false)
            ])
        ])},
        {test: '\'\'\'nil', result: new List([
            new Sym('quote'),
            new List([
                new Sym('quote'),
                new Bool(false)
            ])
        ])},

        {test: '(quote (repeat 1))', result: new List([
            new Sym('repeat'),
            new Int(1)
        ])},
        {test: '(quote (repeat 5 "do" "done"))', result: new List([
            new Sym('repeat'),
            new Int(5),
            new Str('do'),
            new Str('done')
        ])},

        {test: '(quote (setq))', result: new List([
            new Sym('setq')
        ])},
        {test: '(quote (setq a 1 b 2.0))', result: new List([
            new Sym('setq'),
            new Sym('a'),
            new Int(1),
            new Sym('b'),
            new Real(2.0)
        ])},

        {test: '(quote (while nil))', result: new List([
            new Sym('while'),
            new Bool(false)
        ])},
        {test: '(quote (while nil "done"))', result: new List([
            new Sym('while'),
            new Bool(false),
            new Str('done')
        ])},
    ],

    errors: [
    ]
})

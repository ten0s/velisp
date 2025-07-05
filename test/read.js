import {TestRunner} from './test-runner.js'
import {Bool, Int, Real, Str, Sym, List, Pair} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'read',

    tests: [
        {test: '(read)', result: new Bool(false)},
        {test: '(read "")', result: new Bool(false)},
        {test: '(read "nil")', result: new Bool(false)},
        {test: '(read "()")', result: new Bool(false)},

        {test: '(read "1")', result: new Int(1)},
        {test: '(read "2.0")', result: new Real(2.0)},
        {test: '(read "\\\"three\\\"")', result: new Str('three')},
        {test: '(read "id")', result: new Sym('id')},

        {test: '(read "(abc 1 2.0 id)")', result: new List([
            new Sym('abc'), new Int(1), new Real(2.0), new Sym('id')
        ])},

        {test: '(read "(and)")', result: new List([
            new Sym('and')
        ])},
        {test: '(read "(and T nil)")', result: new List([
            new Sym('and'), new Bool(true), new Bool(false)
        ])},

        {test: '(read "(cond)")', result: new List([
            new Sym('cond')
        ])},
        {test: '(read "(cond (nil))")', result: new List([
            new Sym('cond'),
            new List([new Bool(false)])
        ])},
        {test: '(read "(cond (T))")', result: new List([
            new Sym('cond'),
            new List([new Bool(true)])
        ])},
        {test: '(read "(cond (T 1))")', result: new List([
            new Sym('cond'),
            new List([new Bool(true), new Int(1)])
        ])},
        {test: '(read "(cond (T 1 2))")', result: new List([
            new Sym('cond'),
            new List([new Bool(true), new Int(1), new Int(2)])
        ])},
        {test: '(read "(cond (nil 0))")', result: new List([
            new Sym('cond'),
            new List([new Bool(false), new Int(0)])
        ])},
        {test: '(read "(cond (nil 0) (T 1))")', result: new List([
            new Sym('cond'),
            new List([new Bool(false), new Int(0)]),
            new List([new Bool(true), new Int(1)]),
        ])},
        {test: '(read "(cond ((= 0 1) \\\"no\\\") ((= 1 1) \\\"yes\\\"))")', result: new List([
            new Sym('cond'),
            new List([new List([new Sym('='), new Int(0), new Int(1)]), new Str('no')]),
            new List([new List([new Sym('='), new Int(1), new Int(1)]), new Str('yes')]),
        ])},

        {test: '(read "(defun foo () \\\"foo\\\")")', result: new List([
            new Sym('defun'),
            new Sym('foo'),
            new Sym('('),
            new Sym(')'),
            new Str('foo'),
        ])},
        {test: '(read "(defun foo ( / ) \\\"foo\\\")")', result: new List([
            new Sym('defun'),
            new Sym('foo'),
            new Sym('('),
            new Sym(')'),
            new Str('foo'),
        ])},
        {test: '(read "(defun id (x) x)")', result: new List([
            new Sym('defun'),
            new Sym('id'),
            new Sym('('),
            new Sym('x'),
            new Sym(')'),
            new Sym('x'),
        ])},
        {test: '(read "(defun id (x / a) x)")', result: new List([
            new Sym('defun'),
            new Sym('id'),
            new Sym('('),
            new Sym('x'),
            new Sym('/'),
            new Sym('a'),
            new Sym(')'),
            new Sym('x'),
        ])},

        {test: '(read "(foreach n ())")', result: new List([
            new Sym('foreach'),
            new Sym('n'),
            new Bool(false)
        ])},
        {test: '(read "(foreach n (list 1 2 3))")', result: new List([
            new Sym('foreach'),
            new Sym('n'),
            new List([new Sym('list'), new Int(1), new Int(2), new Int(3)])
        ])},
        {test: '(read "(foreach n (list 1 2 3) n)")', result: new List([
            new Sym('foreach'),
            new Sym('n'),
            new List([new Sym('list'), new Int(1), new Int(2), new Int(3)]),
            new Sym('n')
        ])},

        {test: '(read "(function car)")', result: new List([
            new Sym('function'),
            new Sym('car'),
        ])},
        {test: '(read "(function (lambda (x) x))")', result: new List([
            new Sym('function'),
            new List([
                new Sym('lambda'),
                new Sym('('),
                new Sym('x'),
                new Sym(')'),
                new Sym('x')
            ])
        ])},

        {test: '(read "(if T 1 0)")', result: new List([
            new Sym('if'),
            new Bool(true),
            new Int(1),
            new Int(0)
        ])},
        {test: '(read "(if T 1)")', result: new List([
            new Sym('if'),
            new Bool(true),
            new Int(1)
        ])},
        {test: '(read "(if (= 1 1) \\\"yes\\\" \\\"no\\\")")', result: new List([
            new Sym('if'),
            new List([new Sym('='), new Int(1), new Int(1)]),
            new Str('yes'),
            new Str('no')
        ])},

        {test: '(read "(lambda () \\\"foo\\\")")', result: new List([
            new Sym('lambda'),
            new Sym('('),
            new Sym(')'),
            new Str('foo')
        ])},
        {test: '(read "(lambda ( / ) \\\"foo\\\")")', result: new List([
            new Sym('lambda'),
            new Sym('('),
            new Sym(')'),
            new Str('foo')
        ])},
        {test: '(read "(lambda (x) x)")', result: new List([
            new Sym('lambda'),
            new Sym('('),
            new Sym('x'),
            new Sym(')'),
            new Sym('x')
        ])},
        {test: '(read "(lambda (x / a) x)")', result: new List([
            new Sym('lambda'),
            new Sym('('),
            new Sym('x'),
            new Sym('/'),
            new Sym('a'),
            new Sym(')'),
            new Sym('x'),
        ])},

        {test: '(read "(or)")', result: new List([
            new Sym('or')
        ])},
        {test: '(read "(or T nil)")', result: new List([
            new Sym('or'), new Bool(true), new Bool(false)
        ])},

        {test: '(read "(progn)")', result: new List([
            new Sym('progn')
        ])},
        {test: '(read "(progn 1)")', result: new List([
            new Sym('progn'),
            new Int(1)
        ])},
        {test: '(read "(progn 1 2 (+ 1 2))")', result: new List([
            new Sym('progn'),
            new Int(1),
            new Int(2),
            new List([new Sym('+'), new Int(1), new Int(2)])
        ])},

        {test: '(read "(quote nil)")', result: new List([
            new Sym('quote'),
            new Bool(false)
        ])},
        {test: '(read "(quote (1 2 3))")', result: new List([
            new Sym('quote'),
            new List([new Int(1), new Int(2), new Int(3)])
        ])},
        {test: '(read "(quote (list 1 2 3))")', result: new List([
            new Sym('quote'),
            new List([new Sym('list'), new Int(1), new Int(2), new Int(3)])
        ])},
        {test: '(read "(quote (1 . 2))")', result: new List([
            new Sym('quote'),
            new Pair(new Int(1), new Int(2))
        ])},
        {test: '(read "(quote (1 2 . 3))")', result: new List([
            new Sym('quote'),
            new Pair(new Int(1), new Pair(new Int(2), new Int(3)))
        ])},

        {test: '(read "(repeat 1)")', result: new List([
            new Sym('repeat'),
            new Int(1)
        ])},
        {test: '(read "(repeat 5 \\\"done\\\")")', result: new List([
            new Sym('repeat'),
            new Int(5),
            new Str('done')
        ])},
        {test: '(read "(repeat 5 \\\"do\\\" \\\"done\\\")")', result: new List([
            new Sym('repeat'),
            new Int(5),
            new Str('do'),
            new Str('done')
        ])},

        {test: '(read "(setq)")', result: new List([
            new Sym('setq')
        ])},
        {test: '(read "(setq a 1)")', result: new List([
            new Sym('setq'),
            new Sym('a'),
            new Int(1)
        ])},
        {test: '(read "(setq a 1 b 2.0 c \\\"three\\\")")', result: new List([
            new Sym('setq'),
            new Sym('a'),
            new Int(1),
            new Sym('b'),
            new Real(2.0),
            new Sym('c'),
            new Str('three')
        ])},

        {test: '(read "(while nil)")', result: new List([
            new Sym('while'),
            new Bool(false)
        ])},
        {test: '(read "(while nil \\\"done\\\")")', result: new List([
            new Sym('while'),
            new Bool(false),
            new Str('done')
        ])},
        {test: '(read "(while (<= n 10) (setq n (1+ n)) \\\"done\\\")")', result: new List([
            new Sym('while'),
            new List([new Sym('<='), new Sym('n'), new Int(10)]),
            new List([new Sym('setq'), new Sym('n'), new List([new Sym('1+'), new Sym('n')])]),
            new Str('done')
        ])},

        {test: '(read "\'nil")', result: new List([
            new Sym('quote'),
            new Bool(false)
        ])},
        {test: '(read "\'(1 2 3)")', result: new List([
            new Sym('quote'),
            new List([new Int(1), new Int(2), new Int(3)])
        ])},
        {test: '(read "\'(list 1 2 3)")', result: new List([
            new Sym('quote'),
            new List([new Sym('list'), new Int(1), new Int(2), new Int(3)])
        ])},
        {test: '(read "\'(1 . 2)")', result: new List([
            new Sym('quote'),
            new Pair(new Int(1), new Int(2))
        ])},

        {test: '(read "(1 . 2)")', result:
           new Pair(new Int(1), new Int(2))
        },
        {test: '(read "(1 2 . 3)")', result:
            new Pair(new Int(1), new Pair(new Int(2), new Int(3)))
        },

        {test: '(read "(list)")', result: new List([
            new Sym('list')
        ])},
        {test: '(read "(list 1 2 3)")', result: new List([
            new Sym('list'),
            new Int(1),
            new Int(2),
            new Int(3)
        ])},

        // First result only!
        {test: '(read "1 2.0 \\\"three\\\"")', result: new Int(1)},
        {test: '(read "1 . 2")', result: new Int(1)},
    ],

    errors: [
        {test: '(read "" "")', result: new Error('read: too many arguments')},
        {test: '(read nil)', result: new Error('read: expected Str')},
    ]
})

import {TestRunner} from './test-runner.js'
import {Bool, Int, Real, Str, Sym, List, KFun, UFun} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'eval',

    tests: [
        {test: '(eval nil)', result: new Bool(false)},
        {test: '(eval ())', result: new Bool(false)},
        {test: '(eval \'())', result: new List([])},
        {test: '(eval T)', result: new Bool(true)},

        {test: '(eval 1)', result: new Int(1)},
        {test: '(eval 2.0)', result: new Real(2.0)},
        {test: '(eval "three")', result: new Str('three')},

        {test: '(eval \'unknown)', result: new Bool(false)},
        {test: '(setq known \'hello) (eval \'known)', result: new Sym('hello')},

        {test: '(eval \'cos)', result: (act) => {
            return act instanceof KFun
        }},
        {test: '(eval (defun id (x) x))', result: (act) => {
            return act instanceof UFun
        }},
        {test: '(eval (lambda (x) x))', result: (act) => {
            return act instanceof UFun
        }},

        {test: '(eval \'(list))', result: new List([])},
        {test: '(eval \'(list 1 2.0 "three"))', result: new List([
            new Int(1),
            new Real(2.0),
            new Str('three')
        ])},
        {test: '(eval \'(+))', result: new Int(0)},
        {test: '(eval \'(+ 1 2 3))', result: new Int(6)},

        {test: '(eval \'(list (list 1) (list 2)))', result: new List([
            new List([new Int(1)]), new List([new Int(2)])
        ])},

        {test: '(eval \'(list (read "1") (+ 1 1) (+ 1 1 1)))', result: new List([
            new Int(1), new Int(2), new Int(3)
        ])},

        {test: '(setq x 5) (eval \'(+ x 1))', result: new Int(6)},

    ],

    errors: [
        {test: '(eval)', result: new Error('eval: too few arguments')},
        {test: '(eval 1 2)', result: new Error('eval: too many arguments')},
        {test: '(eval \'(abc 1 2))', result: new Error('eval: no such function ABC')},
    ]
})

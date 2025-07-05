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

        // AND special form
        {test: '(eval (read "(and)"))', result: new Bool(true)},
        {test: '(eval (read "(and nil)"))', result: new Bool(false)},
        {test: '(eval (read "(and T)"))', result: new Bool(true)},
        {test: '(eval (read "(and nil nil)"))', result: new Bool(false)},
        {test: '(eval (read "(and T T)"))', result: new Bool(true)},
        {test: '(eval (read "(and T T nil)"))', result: new Bool(false)},
        // Short circuit
        /*
        {test: `(setq a 0 b 0 c 0)
                (and (progn (setq a 1) nil)
                     (progn (setq b 1) T)
                     (progn (setq c 1) T))
                (list a b c)`, result: new List([
            new Int(1), new Int(0), new Int(0)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (and (progn (setq a 1) T)
                     (progn (setq b 1) nil)
                     (progn (setq c 1) T))
                 (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(0)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (and (progn (setq a 1) T)
                     (progn (setq b 1) T)
                     (progn (setq c 1) nil))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(1)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (and (progn (setq a 1) T)
                     (progn (setq b 1) T)
                     (progn (setq c 1) T))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(1)
        ])},
        */

        // IF special form
        {test: '(eval (read "(if T \\\"yes\\\" \\\"no\\\")"))', result: new Str('yes')},
        {test: '(eval (read "(if nil \\\"yes\\\" \\\"no\\\")"))', result: new Str('no')},
        {test: '(eval (read "(if nil \\\"yes\\\")"))', result: new Bool(false)},
        // Short circuit
        // TODO

        // OR special form
        {test: '(eval (read "(or)"))', result: new Bool(false)},
        {test: '(eval (read "(or nil)"))', result: new Bool(false)},
        {test: '(eval (read "(or T)"))', result: new Bool(true)},
        {test: '(eval (read "(or nil nil)"))', result: new Bool(false)},
        {test: '(eval (read "(or T T)"))', result: new Bool(true)},
        {test: '(eval (read "(or nil T)"))', result: new Bool(true)},
        // Short circuit
        // TODO

        // SETQ special for
        {test: '(eval (read "(setq)"))', result: new Bool(false)},
        {test: '(eval (read "(setq a 1)"))', result: new Int(1)},
        {test: '(eval (read "(setq a 1 b 2.0)"))', result: new Real(2.0)},
        {test: '(eval (read "(setq a 1 b 2.0)")) (list a b)', result: new List([
            new Int(1), new Real(2.0)
        ])},
    ],

    errors: [
        {test: '(eval)', result: new Error('eval: too few arguments')},
        {test: '(eval 1 2)', result: new Error('eval: too many arguments')},
        {test: '(eval \'(abc 1 2))', result: new Error('eval: no such function ABC')},
    ]
})

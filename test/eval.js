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
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(and (progn (setq a 1) nil)
                                  (progn (setq b 1) T)
                                  (progn (setq c 1) T))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(0), new Int(0)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(and (progn (setq a 1) T)
                                  (progn (setq b 1) nil)
                                  (progn (setq c 1) T))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(0)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(and (progn (setq a 1) T)
                                  (progn (setq b 1) T)
                                  (progn (setq c 1) nil))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(1)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(and (progn (setq a 1) T)
                                  (progn (setq b 1) T)
                                  (progn (setq c 1) T))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(1)
        ])},

        // COND special form
        {test: '(eval (read "(cond)"))', result: new Bool(false)},
        {test: '(eval (read "(cond (nil))"))', result: new Bool(false)},
        {test: '(eval (read "(cond (nil) (T))"))', result: new Bool(true)},
        {test: '(eval (read "(cond (nil 0))"))', result: new Bool(false)},
        {test: '(eval (read "(cond (nil) (T 1))"))', result: new Int(1)},
        {test: '(eval (read "(cond (T 1 2 3))"))', result: new Int(3)},
        {test: '(eval (read "(cond ((= 0 1) \\\"no\\\") ((= 1 1) \\\"yes\\\"))"))', result: new Str('yes')},
        // Short circuit
        {test: `(setq a 0 b 0)
                (eval (read "(cond (nil (setq a 1))
                                   (T (setq b 1)))"))
                (list a b)`, result: new List([
            new Int(0), new Int(1)
        ])},
        {test: `(setq a 0 b 0)
                (eval (read "(cond (T (setq a 1))
                                   (nil (setq b 1)))"))
                (list a b)`, result: new List([
            new Int(1), new Int(0)
        ])},

        // DEFUN special form
        {test: '(eval (read "(defun foo () 42)"))', result: new Sym('foo')},
        {test: '((eval (read "(defun foo () 42)")))', result: new Int(42)},
        {test: '(eval (read "(defun foo () 42)")) (foo)', result: new Int(42)},
        {test: '(eval (read "(defun id (x) x)"))', result: new Sym('id')},
        {test: '((eval (read "(defun id (x) x)")) 42)', result: new Int(42)},
        {test: '(eval (read "(defun id (x) x)")) (id 42)', result: new Int(42)},
        {test: '(eval (read "(defun id (x / a) x)"))', result: new Sym('id')},

        // FOREACH special form
        {test: '(eval (read "(foreach n (list))"))', result: new Bool(false)},
        {test: '(eval (read "(foreach n \'())"))', result: new Bool(false)},
        {test: '(eval (read "(foreach n ())"))', result: new Bool(false)},
        {test: '(eval (read "(foreach n nil)"))', result: new Bool(false)},
        {test: '(eval (read "(foreach n (list 1 2 3))"))', result: new Bool(false)},
        {test: '(eval (read "(foreach n (list 1 2 3) n)"))', result: new Int(3)},
        {test: '(eval (read "(foreach n (list 1 2 3) (+ n 1))"))', result: new Int(4)},

        // FUNCTION special form
        {test: '(eval (read "(function car)"))', result: new Sym('car')},
        {test: '(eval (read "(function (defun id (x) x))"))', result: new Sym('id')},
        {test: '(eval (read "(function (defun id (x) x))")) (id 42)', result: new Int(42)},
        {test: '(eval (read "(function (lambda (x) x))"))', result: (act) => {
            return act instanceof UFun
        }},
        {test: '((eval (read "(function (lambda (x) x))")) 42)', result: new Int(42)},

        // IF special form
        {test: '(eval (read "(if T \\\"yes\\\" \\\"no\\\")"))', result: new Str('yes')},
        {test: '(eval (read "(if nil \\\"yes\\\" \\\"no\\\")"))', result: new Str('no')},
        {test: '(eval (read "(if nil \\\"yes\\\")"))', result: new Bool(false)},
        // Short circuit
        {test: `(setq a 0 b 0)
                (eval (read "(if T (setq a 1)
                                   (setq b 1))"))
                (list a b)`, result: new List([
            new Int(1), new Int(0)
        ])},
        {test: `(setq a 0 b 0)
                (eval (read "(if nil (setq a 1)
                                     (setq b 1))"))
                (list a b)`, result: new List([
            new Int(0), new Int(1)
        ])},

        // LAMBDA special form
        {test: '(eval (read "(lambda () 42)"))', result: (act) => {
            return act instanceof UFun
        }},
        {test: '((eval (read "(lambda () 42)")))', result: new Int(42)},
        {test: '(eval (read "(lambda (x) x)"))', result: (act) => {
            return act instanceof UFun
        }},
        {test: '((eval (read "(lambda (x) x)")) 42)', result: new Int(42)},
        {test: '(eval (read "(lambda (x / a) x)"))', result: (act) => {
            return act instanceof UFun
        }},
        {test: '(eval \'((lambda (x) (+ x 1)) 5))', result: new Int(6)},

        // OR special form
        {test: '(eval (read "(or)"))', result: new Bool(false)},
        {test: '(eval (read "(or nil)"))', result: new Bool(false)},
        {test: '(eval (read "(or T)"))', result: new Bool(true)},
        {test: '(eval (read "(or nil nil)"))', result: new Bool(false)},
        {test: '(eval (read "(or T T)"))', result: new Bool(true)},
        {test: '(eval (read "(or nil T)"))', result: new Bool(true)},
        // Short circuit
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(or (progn (setq a 1) nil)
                                 (progn (setq b 1) T)
                                 (progn (setq c 1) T))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(0)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(or (progn (setq a 1) nil)
                                 (progn (setq b 1) nil)
                                 (progn (setq c 1) T))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(1)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(or (progn (setq a 1) nil)
                                 (progn (setq b 1) nil)
                                 (progn (setq c 1) nil))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(1), new Int(1)
        ])},
        {test: `(setq a 0 b 0 c 0)
                (eval (read "(or (progn (setq a 1) T)
                                 (progn (setq b 1) nil)
                                 (progn (setq c 1) nil))"))
                (list a b c)`, result: new List([
            new Int(1), new Int(0), new Int(0)
        ])},

        // PROGN special form
        {test: '(eval (read "(progn)"))', result: new Bool(false)},
        {test: '(eval (read "(progn 1)"))', result: new Int(1)},
        {test: '(eval (read "(progn 1 2 (+ 1 2))"))', result: new Int(3)},
        {test: `(setq a 0 b 0)
                (eval (read "(progn (setq a 1) (setq b 2))"))
                (list a b)`, result: new List([
            new Int(1), new Int(2)
        ])},

        // QUOTE special form
        {test: '(eval (read "(quote nil)"))', result: new Bool(false)},
        {test: '(eval (read "\'nil"))', result: new Bool(false)},
        {test: '(eval (read "(quote T)"))', result: new Bool(true)},
        {test: '(eval (read "\'T"))', result: new Bool(true)},
        {test: '(eval (read "(quote 1)"))', result: new Int(1)},
        {test: '(eval (read "\'1"))', result: new Int(1)},
        {test: '(eval (read "(quote 2.0)"))', result: new Real(2.0)},
        {test: '(eval (read "\'2.0"))', result: new Real(2.0)},
        {test: '(eval (read "(quote \\\"three\\\")"))', result: new Str('three')},
        {test: '(eval (read "\'\\\"three\\\""))', result: new Str('three')},
        {test: '(eval (read "(quote foo)"))', result: new Sym('foo')},
        {test: '(eval (read "\'foo"))', result: new Sym('foo')},
        {test: '(eval (read "(quote ())"))', result: new Bool(false)},
        {test: '(eval (read "\'()"))', result: new Bool(false)},
        {test: '(eval (read "(quote (list))"))', result: new List([
            new Sym('list')
        ])},
        {test: '(eval (read "\'(list)"))', result: new List([
            new Sym('list')
        ])},
        {test: '(eval (read "(quote (list 1 2 3))"))', result: new List([
            new Sym('list'), new Int(1), new Int(2), new Int(3)
        ])},
        {test: '(eval (read "\'(list 1 2 3)"))', result: new List([
            new Sym('list'), new Int(1), new Int(2), new Int(3)
        ])},

        // REPEAT special form
        {test: '(eval (read "(repeat 0)"))', result: new Bool(false)},
        {test: '(eval (read "(repeat 5)"))', result: new Bool(false)},
        {test: '(eval (read "(repeat 5 \'done)"))', result: new Sym('done')},
        {test: '(eval (read "(repeat 5 \'do \'done)"))', result: new Sym('done')},
        {test: `(setq a 10 b 100)
                (eval
                    (read "(repeat 4
                               (setq a (+ a 10))
                               (setq b (+ b 100)))"))`, result: new Int(500)},


        // SETQ special form
        {test: '(eval (read "(setq)"))', result: new Bool(false)},
        {test: '(eval (read "(setq a 1)"))', result: new Int(1)},
        {test: '(eval (read "(setq a 1 b 2.0)"))', result: new Real(2.0)},
        {test: '(eval (read "(setq a 1 b 2.0)")) (list a b)', result: new List([
            new Int(1), new Real(2.0)
        ])},

        // WHILE special form
        {test: '(eval (read "(while nil)"))', result: new Bool(false)},
        {test: '(eval (read "(while nil \'done)"))', result: new Bool(false)},
        {test: `(setq test 1)
                (eval (read "(while (<= test 10)
                                 (setq test (+ 1 test)))"))`, result: new Int(11)},
        {test: `(setq test 1)
                (eval (read "(while (<= test 10)
                                 (setq test (+ 1 test))
                                 \'done)"))`, result: new Sym('done')},
    ],

    errors: [
        {test: '(eval)', result: new Error('eval: too few arguments')},
        {test: '(eval 1 2)', result: new Error('eval: too many arguments')},
        {test: '(eval \'(abc 1 2))', result: new Error('eval: no such function ABC')},
        {test: '(setq lst "1 2 3") (eval (read "(foreach n lst)"))', result:
         new Error('eval: foreach: `list` expected List')},
        {test: '(eval (read "(function 1)"))', result:
          new Error('eval: function: expected Sym, Fun')},
        {test: '(eval (read "(repeat nil)"))', result:
         new Error('eval: repeat: `num` expected non-negative Int')},
    ]
})

import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Int, Str, Sym, List} from '../src/VeLispTypes.js'

const tests = [
    {test: '(defun foo () "foo")', result: new Sym('foo')},
    {test: '(Defun foo () "foo")', result: new Sym('foo')},
    {test: '(DEfun foo () "foo")', result: new Sym('foo')},
    {test: '(DEFun foo () "foo")', result: new Sym('foo')},
    {test: '(DEFUn foo () "foo")', result: new Sym('foo')},
    {test: '(DEFUN foo () "foo")', result: new Sym('foo')},

    {test: '(defun foo () "foo") (foo)', result: new Str('foo')},
    {test: '(defun foo ( ) "foo") (foo)', result: new Str('foo')},
    {test: '(defun foo (/) "foo") (foo)', result: new Str('foo')},
    {test: '(defun foo (/ ) "foo") (foo)', result: new Str('foo')},
    {test: '(defun foo ( /) "foo") (foo)', result: new Str('foo')},
    {test: '(defun foo ( / ) "foo") (foo)', result: new Str('foo')},

    {test: '(defun c:foo ( / ) "foo") (c:foo)', result: new Str('foo')},

    {test: '(defun id (x) x) (id "me")', result: new Str('me')},
    {test: '(defun id (x ) x) (id "me")', result: new Str('me')},
    {test: '(defun id ( x) x) (id "me")', result: new Str('me')},
    {test: '(defun id ( x ) x) (id "me")', result: new Str('me')},
    {test: '(defun id (x /) x) (id "me")', result: new Str('me')},
    {test: '(defun id (x / ) x) (id "me")', result: new Str('me')},

    {test: '(defun id (x / ) x) (id "me")', result: new Str('me')},
    {test: '(defun id (x) x) (id (+ 1 2))', result: new Int(3)},

    // The a param is in the local context
    {test: '(defun a (a) (+ a 1)) (a 1)', result: new Int(2)},
    // The a fun and a param are in the local contexts
    {test: `(defun a (x / a)
              (defun a (a)
                (+ a 1))
              (a x))
            (a 1)`,
    result: new Int(2)},
    // Local fun is in the global context (not yet defined)
    {test: '(defun a () (defun b () \'b)) (type b)', result: new Bool(false)},
    // Local fun is in the global context (defined)
    {test: '(defun a () (defun b () \'b)) (a) (type b)', result: new Sym('subr')},
    // Local fun is in the local context (not yet defined)
    {test: '(defun a ( / b) (defun b () \'b)) (type b)', result: new Bool(false)},
    // Local fun is in the local context (defined)
    {test: '(defun a ( / b) (defun b () \'b)) (a) (type b)', result: new Bool(false)},

    {test: '(defun plus (n1 n2) (+ n1 n2)) (plus 1 4)', result: new Int(5)},

    {test: `(setq n1 "one" n2 "two")
            (defun plus (n1 n2) (+ n1 n2))
            (plus 1 4)
            n1`,
    result: new Str('one')},

    {test: `(defun fac (n)
              (cond ((= n 0) 1)
                    (t (* n (fac (- n 1))))))
            (fac 5)`,
    result: new Int(120)},

    {test: `(defun fib (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (t (+ (fib (- n 2))
                          (fib (- n 1))))))
            (fib 10)`,
    result: new Int(55)},

    {test: `(defun fib (n / fib-iter)
              (defun fib-iter (a b counter)
                (if (= counter 0)
                  a
                  (fib-iter b (+ a b) (- counter 1))))
              (fib-iter 0 1 n))
            (fib 10)`,
    result: new Int(55)},

    {test: `(defun double (x)
              (* 2 x))
            (defun map-apply (fn lst)
              (cond ((equal lst nil) nil)
                    (T (cons (apply fn (list (car lst)))
                             (map-apply fn (cdr lst))))))
            (map-apply 'double (list 1 2 3))`,
    result: new List([new Int(2), new Int(4), new Int(6)])},

    {test: `(defun 1+ (num) (+ num 1))
            (defun map (fn lst)
              (cond ((equal lst nil) nil)
                    (T (cons (fn (car lst))
                             (map fn (cdr lst))))))
            (map '1+ (list 1 2 3))`,
    result: new List([new Int(2), new Int(3), new Int(4)])},
]

const errors = [
    {test: '(unknown)', result: new Error('unknown: function not defined')},
]

QUnit.test('defun', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

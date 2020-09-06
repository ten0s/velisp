import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int, Str, Sym} from '../AutoLISPTypes.js';

const tests = [
    {test: '(defun foo () "foo")', result: new Sym('foo')},
    {test: '(defun foo () "foo") (foo)', result: new Str('foo')},
    {test: '(defun id (x) x) (id "me")', result: new Str('me')},
    {test: '(defun id (x) x) (id (+ 1 2))', result: new Int(3)},
    {test: '(defun plus (n1 n2) (+ n1 n2)) (plus 1 4)', result: new Int(5)},
    {test: `(setq n1 "one" n2 "two")
            (defun plus (n1 n2) (+ n1 n2))
            (plus 1 4)
            n1`, result: new Str('one')},
    {test: `(defun fac (n)
              (cond ((= n 0) 1)
                    (t (* n (fac (- n 1))))))
            (fac 5)`, result: new Int(120)},
    {test: `(defun fib (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (t (+ (fib (- n 2))
                          (fib (- n 1))))))
            (fib 10)`, result: new Int(55)},
    {test: `(defun fib (n)
              (defun fib-iter (a b counter)
                (if (= counter 0)
                  a
                  (fib-iter b (+ a b) (- counter 1))))
              (fib-iter 0 1 n))
            (fib 10)`, result: new Int(55)},
];

QUnit.test("defun", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

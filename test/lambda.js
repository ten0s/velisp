const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Int, Str, Sym, List} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(apply (lambda () "foo") (list))', result: new Str('foo')},
    {test: '(apply \'(lambda () "foo") (list))', result: new Str('foo')},
    {test: '(apply (lambda (x) x) (list "me"))', result: new Str('me')},
    {test: '(apply \'(lambda (x) x) (list "me"))', result: new Str('me')},

    {test: `(defun map-apply (fn lst)
              (cond ((equal lst nil) nil)
                    (T (cons (apply fn (list (car lst)))
                             (map-apply fn (cdr lst))))))
            (map-apply (lambda (x) (* 2 x)) (list 1 2 3))`,
     result: new List([new Int(2), new Int(4), new Int(6)])},

    {test: `(defun map (fn lst)
              (cond ((equal lst nil) nil)
                    (T (cons (fn (car lst))
                             (map fn (cdr lst))))))
            (map (lambda (x) (1+ x)) (list 1 2 3))`,
     result: new List([new Int(2), new Int(3), new Int(4)])},
];

QUnit.test("lambda", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

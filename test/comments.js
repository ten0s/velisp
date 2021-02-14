const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Int} = require('../src/VeLispTypes.js')

const tests = [
    {test: `;;; Comment #1
            (defun fib (n)
              ;; Comment #2
              (fib-iter 0 1 n)) ; Comment #4

            (defun fib-iter (a b counter)
              ;; Comment #5
              (if (= counter 0)
                a ;| Comment #7 |;
                (fib-iter b (+ a b) (- counter 1))))

            ;| Comment #8
            (princ (fib 1000)) ; Comment #9
            Comment #10
            |;
            (princ (fib 10))`, result: new Int(55)},
]

QUnit.test('comments', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

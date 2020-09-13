const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Pair} = require('../src/VeLispTypes.js');

const tests = [
    // TODO: (eq & (equal tests
    {test: '(= nil)', result: new Bool(true)},
    {test: '(= T)', result: new Bool(true)},

    {test: '(= 1)', result: new Bool(true)},
    {test: '(= 1.0)', result: new Bool(true)},
    {test: '(= "1.0")', result: new Bool(true)},

    // TODO: (eq & (equal tests
    {test: '(= nil nil))', result: new Bool(true)},
    {test: '(= nil T))', result: new Bool(false)},
    {test: '(= T nil))', result: new Bool(false)},
    {test: '(= T T))', result: new Bool(true)},

    {test: '(= 1 1))', result: new Bool(true)},
    {test: '(= 4 4.0)', result: new Bool(true)},
    {test: '(= 20 388)', result: new Bool(false)},
    {test: '(= 2.4 2.4 2.4)', result: new Bool(true)},
    {test: '(= 499 499 500)', result: new Bool(false)},

    {test: '(= "me" "me"))', result: new Bool(true)},
    {test: '(= "me" "you"))', result: new Bool(false)},

    // All args evaluated eagerly
    {test: `(setq a 0)
            (cons (= 1 2 (progn (setq a 1) 2)) a)`,
     result: new Pair(new Bool(false), new Int(1))},

    // TODO: (eq & (equal tests
    {test: '(= (list) (list))', result: new Bool(true)},
    {test: '(= (list) nil)', result: new Bool(true)},
    {test: '(= nil (list))', result: new Bool(true)},
    {test: '(= (list 1 2 3) (list 1 2 3))', result: new Bool(true)},
    {test: '(= (list 1 2 3) (list 1 2 3 4))', result: new Bool(false)},

    // TODO: (eq & (equal tests
    {test: '(= (cons 1 \'a) (cons 1 \'a))', result: new Bool(true)},
    {test: '(= (cons 1 \'a) (cons 2 \'a))', result: new Bool(false)},
    {test: '(= (cons 1 \'a) (cons 1 \'b))', result: new Bool(false)},
];

QUnit.test("equalTo", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

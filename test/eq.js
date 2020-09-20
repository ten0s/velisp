const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Pair} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(eq nil nil)', result: new Bool(true)},
    {test: '(eq T T)', result: new Bool(true)},
    {test: '(eq nil T))', result: new Bool(false)},
    {test: '(eq T nil))', result: new Bool(false)},

    {test: '(eq 1 1))', result: new Bool(true)},
    {test: '(eq 1 1.0)', result: new Bool(true)},
    {test: '(eq 1.0 1)', result: new Bool(true)},
    {test: '(eq 1.0 1.0)', result: new Bool(true)},

    {test: '(eq "foo" "foo"))', result: new Bool(true)},

    {test: '(eq \'foo \'foo))', result: new Bool(true)},
    {test: '(eq \'foo \'FOO))', result: new Bool(true)},

    {test: '(eq (list) (list))', result: new Bool(false)},
    {test: '(eq (list) nil)', result: new Bool(false)},
    {test: '(eq nil (list))', result: new Bool(false)},
    {test: '(eq (list 1 2 3) (list 1 2 3))', result: new Bool(false)},

    {test: '(eq (cons 1 \'a) (cons 1 \'a))', result: new Bool(false)},
    {test: '(eq (cons 1 \'a) (cons 2 \'a))', result: new Bool(false)},

    {test: `(setq f1 (list a b c) f2 (list a b c) f3 f2)
            (eq f1 f1)`, result: new Bool(true)},
    {test: `(setq f1 (list a b c) f2 (list a b c) f3 f2)
            (eq f3 f2)`, result: new Bool(true)},
];

const errors = [
    {test: '(eq)', result: new Error('eq: too few arguments')},
    {test: '(eq 1 2 3)', result: new Error('eq: too many arguments')},
];

QUnit.test("eq", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

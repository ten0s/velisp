const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(>= 1)', result: new Bool(true)},

    {test: '(>= 1 1))', result: new Bool(true)},
    {test: '(>= 1 1.0)', result: new Bool(true)},
    {test: '(>= 1.0 1)', result: new Bool(true)},
    {test: '(>= 1.0 1.0)', result: new Bool(true)},
    {test: '(>= 120 17)', result: new Bool(true)},
    {test: '(>= 3.5 1794)', result: new Bool(false)},
    {test: '(>= 77 4 2)', result: new Bool(true)},
    {test: '(>= 77 4 4)', result: new Bool(true)},
    {test: '(>= 77 4 9)', result: new Bool(false)},
    {test: '(>= "c" "b"))', result: new Bool(true)},
    {test: '(>= "c" "c"))', result: new Bool(true)},
];

const errors = [
    {test: '(>=)', result: new Error('>=: too few arguments')},

    {test: '(>= nil)', result: new Error('>=: expected Int, Real, Str')},
    {test: '(>= T)', result: new Error('>=: expected Int, Real, Str')},
    {test: '(>= (list))', result: new Error('>=: expected Int, Real, Str')},
    {test: '(>= (cons 1 \'a))', result: new Error('>=: expected Int, Real, Str')},
];

QUnit.test("greaterThanOrEqualTo", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(<= 1)', result: new Bool(true)},

    {test: '(<= 1 1))', result: new Bool(true)},
    {test: '(<= 1 1.0)', result: new Bool(true)},
    {test: '(<= 1.0 1)', result: new Bool(true)},
    {test: '(<= 1.0 1.0)', result: new Bool(true)},
    {test: '(<= 120 17)', result: new Bool(false)},
    {test: '(<= 17 120)', result: new Bool(true)},
    {test: '(<= 3.5 1794)', result: new Bool(true)},
    {test: '(<= 2 4 77)', result: new Bool(true)},
    {test: '(<= 4 4 77)', result: new Bool(true)},
    {test: '(<= 2 9 9)', result: new Bool(true)},
    {test: '(<= 2 9 4 5)', result: new Bool(false)},
    {test: '(<= "b" "b"))', result: new Bool(true)},
    {test: '(<= "b" "c"))', result: new Bool(true)},
];

const errors = [
    {test: '(<=)', result: new Error('<=: too few arguments')},
    {test: '(<= nil)', result: new Error('<=: expected Int, Real, Str')},
    {test: '(<= 1 nil)', result: new Error('<=: expected Int, Real, Str')},
];

QUnit.test("lessThanOrEqualTo", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

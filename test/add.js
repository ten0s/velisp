const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Int, Real} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(+)', result: new Int(0)},
    {test: '(+ 1)', result: new Int(1)},
    {test: '(+ 1 2)', result: new Int(3)},
    {test: '(+ 1 2.0)', result: new Real(3.0)},
    {test: '(+ 2.0 1)', result: new Real(3.0)},
    {test: '(+ 1 2 3 4 5)', result: new Int(15)},
    {test: '(+ 2147483646 3))', result: new Int(2147483649)}, // TODO: -2147483647
];

const errors = [
    {test: '(+ 1 "2")', result: new Error('+: expected Int, Real')},
];

QUnit.test("add", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

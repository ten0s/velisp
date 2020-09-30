const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Real} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(sin 1)', result: new Real(Math.sin(1))},
    {test: '(sin 1.0)', result: new Real(Math.sin(1.0))},
    {test: '(sin 0)', result: new Real(Math.sin(0))},
    {test: '(sin 0.0)', result: new Real(Math.sin(0.0))},
];

const errors = [
    {test: '(sin)', result: new Error('sin: too few arguments')},
    {test: '(sin 0 1)', result: new Error('sin: too many arguments')},
];

QUnit.test("sin", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

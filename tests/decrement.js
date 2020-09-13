const QUnit = require('qunit');
const {evaluate} = require('../VeLispEvaluator.js');
const {Int, Real} = require('../VeLispTypes.js');

const tests = [
    {test: '(1- 0)', result: new Int(-1)},
    {test: '(1- 0.0)', result: new Real(-1)},
    {test: '(1- 1)', result: new Int(0)},
    {test: '(1- 1.0)', result: new Real(0)},
    {test: '(1- -1)', result: new Int(-2)},
    {test: '(1- -1.0)', result: new Real(-2)},
];

QUnit.test("decrement", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

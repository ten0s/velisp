const QUnit = require('qunit');
const {evaluate} = require('../VeLispEvaluator.js');
const {Int, Real} = require('../VeLispTypes.js');

const tests = [
    {test: '(-)', result: new Int(0)},
    {test: '(- 1)', result: new Int(-1)},
    {test: '(- 1 2.0)', result: new Real(-1.0)},
    {test: '(- 2.0 1)', result: new Real(1.0)},
    {test: '(- 50 40)', result: new Int(10)},
    {test: '(- 50 40.0)', result: new Real(10.0)},
    {test: '(- 15 1 2 3 4 5)', result: new Int(0)},
];

QUnit.test("subtract", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

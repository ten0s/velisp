const QUnit = require('qunit');
const {evaluate} = require('../VeLispEvaluator.js');
const {Int, Real} = require('../VeLispTypes.js');

const tests = [
    {test: '(*)', result: new Int(0)},
    {test: '(* 2)', result: new Int(2)},
    {test: '(* 2.0)', result: new Real(2.0)},
    {test: '(* 2 3)', result: new Int(6)},
    {test: '(* 2 3.0)', result: new Real(6.0)},
    {test: '(* 2 3 4.0)', result: new Real(24.0)},
    {test: '(* 3 -4.5)', result: new Real(-13.5)},
    {test: '(* 1 2 3 4 5)', result: new Int(120)},
];

QUnit.test("multiply", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

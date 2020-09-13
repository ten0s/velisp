const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(and)', result: new Bool(true)},

    {test: '(and nil)', result: new Bool(false)},
    {test: '(and T)', result: new Bool(true)},

    {test: '(and nil nil)', result: new Bool(false)},
    {test: '(and T T)', result: new Bool(true)},
    {test: '(and T nil)', result: new Bool(false)},
    {test: '(and nil T)', result: new Bool(false)},

    {test: '(and T nil nil)', result: new Bool(false)},
    {test: '(and nil T nil)', result: new Bool(false)},
    {test: '(and nil nil T)', result: new Bool(false)},

    {test: '(and T T T)', result: new Bool(true)},
];

QUnit.test("and", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

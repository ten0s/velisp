const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(or)', result: new Bool(false)},
    {test: '(Or)', result: new Bool(false)},
    {test: '(OR)', result: new Bool(false)},

    {test: '(or nil)', result: new Bool(false)},
    {test: '(or T)', result: new Bool(true)},

    {test: '(or nil nil)', result: new Bool(false)},
    {test: '(or T T)', result: new Bool(true)},
    {test: '(or T nil)', result: new Bool(true)},
    {test: '(or nil T)', result: new Bool(true)},

    {test: '(or T nil nil)', result: new Bool(true)},
    {test: '(or nil T nil)', result: new Bool(true)},
    {test: '(or nil nil T)', result: new Bool(true)},
    {test: '(or nil nil nil)', result: new Bool(false)},
    {test: '(or T T T)', result: new Bool(true)},
];

QUnit.test("or", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

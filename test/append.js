const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Real, Str, List, Pair} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(append)', result: new Bool(false)},
    {test: '(append nil)', result: new List([])},
    {test: '(append ())', result: new List([])},
    {test: '(append \'())', result: new List([])},
    {test: '(append \'(1))', result: new List([new Int(1)])},
    {test: '(append \'(1) \'(2 3))', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
];

const errors = [
    {test: '(append 1)', result: new Error('append: expected List')},
    {test: '(append \'(1) 2)', result: new Error('append: expected List')},
];

QUnit.test("append", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

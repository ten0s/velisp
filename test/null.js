const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(null nil)', result: new Bool(true)},
    {test: '(null T)', result: new Bool(false)},

    {test: '(null 0)', result: new Bool(false)},
    {test: '(null 0.0)', result: new Bool(false)},
    {test: '(null "")', result: new Bool(false)},
    {test: '(null \'foo)', result: new Bool(false)},

    {test: '(null (list))', result: new Bool(true)},
    {test: '(null (list nil))', result: new Bool(false)},

    {test: '(null (cons nil nil))', result: new Bool(false)},
];

const errors = [
    {test: '(null)', result: new Error('null: too few arguments')},
    {test: '(null (list) \'error)', result: new Error('null: too many arguments')},
];

QUnit.test("null", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

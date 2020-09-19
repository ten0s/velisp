const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(not nil)', result: new Bool(true)},
    {test: '(not T)', result: new Bool(false)},

    {test: '(not 0)', result: new Bool(false)},
    {test: '(not 0.0)', result: new Bool(false)},
    {test: '(not "")', result: new Bool(false)},
    {test: '(not \'foo)', result: new Bool(false)},

    {test: '(not (list))', result: new Bool(true)},
    {test: '(not (list nil))', result: new Bool(false)},

    {test: '(not (cons nil nil))', result: new Bool(false)},
];

const errors = [
    {test: '(not)', result: new Error('not: too few arguments')},
    {test: '(not (list) \'error)', result: new Error('not: too many arguments')},
];

QUnit.test("not", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});
const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(getenv "UNKNOWN")', result: new Bool(false)},
    {test: '(getenv "HOME")', result: new Str(process.env['HOME'])},
    {test: '(setenv "EMPTY" "") (getenv "EMPTY")', result: new Str('')},
    {test: '(setenv "NONEMPTY" "value") (getenv "NONEMPTY")', result: new Str('value')},
    {test: '(setenv "NONEMPTY" "value") (getenv "nonempty")', result: new Bool(false)},
];

const errors = [
    {test: '(getenv)', result: new Error('getenv: too few arguments')},
    {test: '(getenv "VAR1" "VAR2")', result: new Error('getenv: too many arguments')},
    {test: '(getenv \'VAR)', result: new Error('getenv: expected Str')},
];

QUnit.test("getenv", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

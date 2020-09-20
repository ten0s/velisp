const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(setenv "NAME" "value")', result: new Str('value')},
    {test: '(setenv "NAME" "")', result: new Str('')},
    {test: '(setenv "NAME" "value") (getenv "NAME")', result: new Str('value')},
];

const errors = [
    {test: '(setenv)', result: new Error('setenv: too few arguments')},
    {test: '(setenv "NAME")', result: new Error('setenv: too few arguments')},
    {test: '(setenv "NAME" "VALUE" "VALUE")', result: new Error('setenv: too many arguments')},
    {test: '(setenv \'NAME "VALUE")', result: new Error('setenv: `varname` expected Str')},
    {test: '(setenv "NAME" \'VALUE)', result: new Error('setenv: `value` expected Str')},
];

QUnit.test("setenv", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Int, Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(apply (defun foo () "foo") (list))', result: new Str('foo')},
    {test: '(apply (defun foo () "foo") nil)', result: new Str('foo')},
    {test: '(apply \'+ (list 1 2 3))', result: new Int(6)},
];

const errors = [
    {test: '(apply)', result: new Error('apply: too few arguments')},
    {test: '(apply \'+)', result: new Error('apply: too few arguments')},
    {test: '(apply \'+ (list 1 2) (list 3 4))', result: new Error('apply: too many arguments')},
    {test: '(apply \'+ 1)', result: new Error('apply: list must be List')},
    {test: '(apply \'add (list 1 2 3))', result: new Error('apply: no such function \'ADD')},
];

QUnit.test("apply", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

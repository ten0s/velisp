const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Real, Str, Sym} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(vl-symbol-value \'t)', result: new Bool(true)},
    {test: '(vl-symbol-value \'nil)', result: new Bool(false)},
    {test: '(vl-symbol-value \'PI)', result: new Real(Math.PI)},
];

const errors = [
    {test: '(vl-symbol-value)', result: new Error('vl-symbol-value: too few arguments')},
    {test: '(vl-symbol-value \'foo \'bar)', result: new Error('vl-symbol-value: too many arguments')},
    {test: '(vl-symbol-value "foo")', result: new Error('vl-symbol-value: expected Sym')},
];
    
QUnit.test("vl-symbol-value", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

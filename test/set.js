const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Real, Sym, List, Pair} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(set \'a 1)', result: new Int(1)},
    {test: '(set \'a 2) (+ a 1)', result: new Int(3)},
    {test: '(set \'a (+ 1 2)) a', result: new Int(3)},
    {test: '(setq a \'b) (set a 1) (cons a b)', result: new Pair(new Sym('b'), new Int(1))},

    {test: '(defun GLOBAL () (set \'a 1)) (GLOBAL) a', result: new Int(1)},
    {test: '(set \'a 1) (defun PARAM (a) (set \'a 2)) (PARAM 3) a', result: new Int(1)},
    {test: '(set \'a 1) (defun LOCAL ( / a) (set \'a 2)) (LOCAL) a', result: new Int(1)},
];

const errors = [
    {test: '(set)', result: new Error('set: too few arguments')},
    {test: '(set \'foo)', result: new Error('set: too few arguments')},
    {test: '(set \'foo \'bar \'baz)', result: new Error('set: too many arguments')},
    {test: '(set "foo" 1)', result: new Error('set: `sym` expected Sym')},
];

QUnit.test("set", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

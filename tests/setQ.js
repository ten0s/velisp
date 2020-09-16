const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Real, List} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(setq)', result: new Bool(false)},
    {test: '(Setq)', result: new Bool(false)},
    {test: '(SEtq)', result: new Bool(false)},
    {test: '(SETq)', result: new Bool(false)},
    {test: '(SETQ)', result: new Bool(false)},

    {test: '(setq a 1)', result: new Int(1)},
    {test: '(setq a 2) (+ a 1)', result: new Int(3)},
    {test: '(setq a 1 b 2 c 3) (+ a b c)', result: new Int(6)},
    {test: '(setq a (+ 1 2)) a', result: new Int(3)},
    {test: '(setq a (progn (* 2 3.0))) a', result: new Real(6.0)},
    {test: '(setq a (list (list) (list 1) (list 1 2))) a',
     result: new List([
         new List([]), new List([new Int(1)]), new List([new Int(1), new Int(2)])
     ])},
    {test: '(defun GLOBAL () (setq a 1)) (GLOBAL) a', result: new Int(1)},
    {test: '(setq a 1) (defun PARAM (a) (setq a 2)) (PARAM 3) a', result: new Int(1)},
    {test: '(setq a 1) (defun LOCAL ( / a) (setq a 2)) (LOCAL) a', result: new Int(1)},
];

QUnit.test("setQ", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

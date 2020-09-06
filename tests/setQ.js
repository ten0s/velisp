import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int} from '../AutoLISPTypes';

const tests = [
    {test: '(setq a 1)', result: new Int(1)},
    {test: '(setq a 2) (+ a 1)', result: new Int(3)},
    {test: '(setq a 1 b 2 c 3) (+ a b c)', result: new Int(6)},
    {test: '(defun GLOBAL () (setq a 1)) (GLOBAL) a', result: new Int(1)},
    {test: '(setq a 1) (defun PARAM (a) (setq a 2)) (PARAM 3) a', result: new Int(1)},
    {test: '(setq a 1) (defun LOCAL ( / a) (setq a 2)) (LOCAL) a', result: new Int(1)},
];

QUnit.test("setQ", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

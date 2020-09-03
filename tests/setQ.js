import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int} from '../AutoLISPTypes';

const tests = [
    {test: '(setq a 1)', result: new Int(1)},
    {test: '(setq a 2) (+ a 1)', result: new Int(3)},
    {test: '(setq a 1 b 2 c 3) (+ a b c)', result: new Int(6)},
];

QUnit.test("setQ", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

import QUnit from 'qunit';
import {evaluate} from '../VeLispEvaluator.js';
import {Bool, Int} from '../VeLispTypes.js';

const tests = [
    {test: '(progn)', result: new Bool(false)},
    {test: '(progn 1)', result: new Int(1)},
    {test: '(progn 1 2 (+ 1 2))', result: new Int(3)},
    {test: `(setq a 0)
            (progn
              (setq a (1- a))
              (setq a (1- a)))`,
     result: new Int(-2)},
];

QUnit.test("progn", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

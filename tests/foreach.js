import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Bool, Int} from '../AutoLISPTypes.js';

const tests = [
    {test: '(while nil "done")', result: new Bool(false)},
    {test: `(setq fac 1)
            (foreach n (list 1 2 3 4 5)
              (setq fac (* n fac)))`, result: new Int(120)},
];

QUnit.test("foreach", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

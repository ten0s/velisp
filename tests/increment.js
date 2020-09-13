import QUnit from 'qunit';
import {evaluate} from '../VeLispEvaluator.js';
import {Int, Real} from '../VeLispTypes.js';

const tests = [
    {test: '(1+ 0)', result: new Int(1)},
    {test: '(1+ 0.0)', result: new Real(1)},
    {test: '(1+ 1)', result: new Int(2)},
    {test: '(1+ 1.0)', result: new Real(2)},
    {test: '(1+ -1)', result: new Int(0)},
    {test: '(1+ -1.0)', result: new Real(0)},
];

QUnit.test("increment", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

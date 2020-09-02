import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Integer, Real} from '../AutoLISPTypes';

const tests = [
    {test: '(-)', result: new Integer(0)},
    {test: '(- 1)', result: new Integer(-1)},
    {test: '(- 1 2.0)', result: new Real(-1.0)},
    {test: '(- 2.0 1)', result: new Real(1.0)},
    {test: '(- 50 40)', result: new Integer(10)},
    {test: '(- 50 40.0)', result: new Real(10.0)},
    {test: '(- 15 1 2 3 4 5)', result: new Integer(0)},
];

QUnit.test("subtract", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});
import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Integer, Real} from '../AutoLISPTypes';

const tests = [
    {test: '(*)', result: new Integer(0)},
    {test: '(* 2)', result: new Integer(2)},
    {test: '(* 2.0)', result: new Real(2.0)},
    {test: '(* 2 3)', result: new Integer(6)},
    {test: '(* 2 3.0)', result: new Real(6.0)},
    {test: '(* 2 3 4.0)', result: new Real(24.0)},
    {test: '(* 3 -4.5)', result: new Real(-13.5)},
    {test: '(* 1 2 3 4 5)', result: new Integer(120)},
];

QUnit.test("multiply", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

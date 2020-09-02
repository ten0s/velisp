import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Integer, Real} from '../AutoLISPTypes';

const tests = [
    {test: '(+)', result: new Integer(0)},
    {test: '(+ 1)', result: new Integer(1)},
    {test: '(+ 1 2)', result: new Integer(3)},
    {test: '(+ 1 2.0)', result: new Real(3.0)},
    {test: '(+ 2.0 1)', result: new Real(3.0)},
    {test: '(+ 1 2 3 4 5)', result: new Integer(15)},
    {test: '(+ 2147483646 3))', result: new Integer(2147483649)}, // TODO: -2147483647
];

QUnit.test("add", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});
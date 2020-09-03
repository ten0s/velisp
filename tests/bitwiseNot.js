import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int} from '../AutoLISPTypes';

const tests = [
    {test: '(~ 0)', result: new Int(-1)},
    {test: '(~ -1)', result: new Int(0)},
    {test: '(~ 3)', result: new Int(-4)},
    {test: '(~ -4)', result: new Int(3)},
    {test: '(~ 100)', result: new Int(-101)},
    {test: '(~ -101)', result: new Int(100)},
];

QUnit.test("bitwiseNot", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

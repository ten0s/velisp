import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Integer, Real, String, List} from '../AutoLISPTypes';

const tests = [
    {test: '2', result: new Integer(2)},
    {test: '2.0', result: new Real(2.0)},
    {test: '"2.0"', result: new String('2.0')},
    {test: '(list)', result: new List([])},
    {test: '(list 1 2 3)', result: new List([
        new Integer(1), new Integer(2), new Integer(3)
    ])},
];

QUnit.test("types", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

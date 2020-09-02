import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Integer, Real, String, List} from '../AutoLISPTypes';

const tests = [
    {test: '(list)', result: new List([])},
    {test: '(list 1 2 3)', result: new List([
        new Integer(1), new Integer(2), new Integer(3)
    ])},
    {test: '(list 1 "2" 3.0 (list 4))', result: new List([
        new Integer(1), new String('2'), new Real(3.0), new List([new Integer(4)])
    ])},
    {test: '(car (list 1 2 3))', result: new Integer(1)},
    {test: '(cdr (list 1 2 3))', result: new List([
        new Integer(2), new Integer(3)
    ])},
];

QUnit.test("list", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

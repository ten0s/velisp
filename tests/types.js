import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int, Real, Str, List} from '../AutoLISPTypes';

const tests = [
    {test: '2', result: new Int(2)},
    {test: '2.0', result: new Real(2.0)},
    {test: '"2.0"', result: new Str('2.0')},
    {test: '(list)', result: new List([])},
    {test: '(list 1 2 3)', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
];

QUnit.test("types", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int, Real} from '../AutoLISPTypes';

const tests = [
    {test: '(/)', result: new Int(0)},
    {test: '(/ 4)', result: new Int(4)},
    {test: '(/ 4.0)', result: new Real(4.0)},
    {test: '(/ 5 2)', result: new Int(2)}, // Integer division
    {test: '(/ 5 2.0)', result: new Real(2.5)},
    {test: '(/ 100 2)', result: new Int(50)}, // Integer division
    {test: '(/ 100 2.0)', result: new Real(50.0)},
    {test: '(/ 100 20.0 2)', result: new Real(2.5)},
    {test: '(/ 100 20 2)', result: new Int(2)}, // Integer division
];

QUnit.test("divide", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

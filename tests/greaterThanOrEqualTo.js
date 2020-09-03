import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Bool} from '../AutoLISPTypes';

const tests = [
    {test: '(>= 1)', result: new Bool(true)},
    // TODO: '(>= nil)' true
    // TODO: '(>= T)' true
    {test: '(>= 1 1))', result: new Bool(true)},
    {test: '(>= 1 1.0)', result: new Bool(true)},
    {test: '(>= 1.0 1)', result: new Bool(true)},
    {test: '(>= 1.0 1.0)', result: new Bool(true)},
    {test: '(>= 120 17)', result: new Bool(true)},
    {test: '(>= 3.5 1794)', result: new Bool(false)},
    {test: '(>= 77 4 2)', result: new Bool(true)},
    {test: '(>= 77 4 4)', result: new Bool(true)},
    {test: '(>= 77 4 9)', result: new Bool(false)},
    {test: '(>= "c" "b"))', result: new Bool(true)},
    {test: '(>= "c" "c"))', result: new Bool(true)},
];

QUnit.test("greaterThanOrEqualTo", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});
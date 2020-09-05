import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Bool, Str} from '../AutoLISPTypes';

const tests = [
    {test: '(cond)', result: new Bool(false)},
    {test: '(cond (nil "no"))', result: new Bool(false)},
    {test: '(cond (T "yes"))', result: new Str('yes')},
    {test: '(cond (nil "no") (T "yes"))', result: new Str('yes')},
    {test: '(cond ((= 0 1) "no") ((= 1 1) "yes"))', result: new Str('yes')},
];

QUnit.test("cond", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

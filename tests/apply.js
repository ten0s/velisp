import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int, Str} from '../AutoLISPTypes.js';

const tests = [
    {test: '(apply (defun foo () "foo") (list))', result: new Str('foo')},
    {test: '(apply \'+ (list 1 2 3))', result: new Int(6)},
];

QUnit.test("apply", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

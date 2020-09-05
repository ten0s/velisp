import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Bool, Int, Str} from '../AutoLISPTypes';

const tests = [
    {test: '(defun foo () "foo") (foo)', result: new Str('foo')},
];

QUnit.test("defun", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Int, Str} from '../AutoLISPTypes';

const tests = [
    {test: '(defun foo () "foo") (foo)', result: new Str('foo')},
    {test: '(defun id (x) x) (id "me")', result: new Str('me')},
    {test: '(defun plus (n1 n2) (+ n1 n2)) (plus 1 4)', result: new Int(5)},
];

QUnit.test("defun", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

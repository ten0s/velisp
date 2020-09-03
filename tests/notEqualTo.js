import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Bool} from '../AutoLISPTypes';

const tests = [
    {test: '(/= 1)', result: new Bool(true)},
    // TODO: '(/= nil)' true
    // TODO: '(/= T)' true
    {test: '(/= 1 1))', result: new Bool(false)},
    {test: '(/= 4 4.0)', result: new Bool(false)},
    {test: '(/= 10 20)', result: new Bool(true)},
    {test: '(/= 20 388)', result: new Bool(true)},
    {test: '(/= 5.43 5.44)', result: new Bool(true)},
    {test: '(/= 2.4 2.4 2.4)', result: new Bool(false)},
    {test: '(/= 499 499 500)', result: new Bool(false)},
    {test: '(/= 10 20 10 20)', result: new Bool(true)},
    {test: '(/= 10 20 10 20 20)', result: new Bool(false)},
    {test: '(/= "me" "me"))', result: new Bool(false)},
];

QUnit.test("notEqualTo", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});
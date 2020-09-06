import QUnit from 'qunit';
import {evaluate} from '../AutoLISPEvaluator.js';
import {Bool, Int, Str} from '../AutoLISPTypes.js';

const tests = [
    {test: '(if T 1 0)', result: new Int(1)},
    {test: '(if nil 1 0)', result: new Int(0)},
    {test: '(if T T nil)', result: new Bool(true)},
    {test: '(if nil T nil)', result: new Bool(false)},
    {test: '(if nil T)', result: new Bool(false)}, // no else

    {test: '(if 0 1 0)', result: new Int(1)}, // 0 is truthy
    {test: '(if 1 1 0)', result: new Int(1)}, // 1 is truthy

    {test: '(if 0.0 1 0)', result: new Int(1)}, // 0.0 is truthy
    {test: '(if 1.0 1 0)', result: new Int(1)}, // 1.0 is truthy

    {test: '(if "" 1 0)', result: new Int(1)}, // "" is truthy
    {test: '(if "0" 1 0)', result: new Int(1)}, // "0" is truthy
    {test: '(if "1" 1 0)', result: new Int(1)}, // "0" is truthy

    {test: '(if (list) 1 0)', result: new Int(0)}, // () is falsy
    {test: '(if (list 1) 1 0)', result: new Int(1)},

    {test: '(if (= 1 1) "yes" "no")', result: new Str('yes')},
    {test: '(if (/= 1 1) "yes" "no")', result: new Str('no')},
    {test: '(if (/= 1 1) "yes")', result: new Bool(false)}, // no else

    {test: '(if (= 2 (+ 1 1)) "yes" "no")', result: new Str('yes')},
    {test: '(if (/= 2 (+ 1 1)) "yes" "no")', result: new Str('no')},
    {test: '(if (/= 2 (+ 1 1)) "yes")', result: new Bool(false)}, // no else
];

QUnit.test("if", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Str, List, Int} from '../src/VeLispTypes.js'

const tests = [
    {test: '(cond)', result: new Bool(false)},
    {test: '(Cond)', result: new Bool(false)},
    {test: '(COnd)', result: new Bool(false)},
    {test: '(CONd)', result: new Bool(false)},
    {test: '(COND)', result: new Bool(false)},

    {test: '(cond (nil))', result: new Bool(false)},
    {test: '(cond (nil) (T))', result: new Bool(true)},
    {test: '(cond (nil) (T "one" "two" "three"))', result: new Str('three')},
    {test: '(cond (nil) (T "one" "two") (T "three"))', result: new Str('two')},
    {test: '(cond (nil "no"))', result: new Bool(false)},
    {test: '(cond (T "yes"))', result: new Str('yes')},
    {test: '(cond (nil "no") (T "yes"))', result: new Str('yes')},
    {test: '(cond ((= 0 1) "no") ((= 1 1) "yes"))', result: new Str('yes')},

    // Short circuit
    {test: `(setq a 0 b 0)
            (cond (nil (setq a 1))
                  (T (setq b 1)))
            (list a b)`, result: new List([
        new Int(0), new Int(1)
    ])},
    {test: `(setq a 0 b 0)
            (cond (T (setq a 1))
                  (nil (setq b 1)))
            (list a b)`, result: new List([
        new Int(1), new Int(0)
    ])},
]

QUnit.test('cond', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

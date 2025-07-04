import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Int, List} from '../src/VeLispTypes.js'

const tests = [
    {test: '(and)', result: new Bool(true)},
    {test: '(And)', result: new Bool(true)},
    {test: '(ANd)', result: new Bool(true)},
    {test: '(AND)', result: new Bool(true)},

    {test: '(and nil)', result: new Bool(false)},
    {test: '(and T)', result: new Bool(true)},

    {test: '(and nil nil)', result: new Bool(false)},
    {test: '(and T T)', result: new Bool(true)},
    {test: '(and T nil)', result: new Bool(false)},
    {test: '(and nil T)', result: new Bool(false)},

    {test: '(and T nil nil)', result: new Bool(false)},
    {test: '(and nil T nil)', result: new Bool(false)},
    {test: '(and nil nil T)', result: new Bool(false)},

    {test: '(and T T T)', result: new Bool(true)},

    // Short circuit
    {test: `(setq a 0 b 0 c 0)
            (and (progn (setq a 1) nil)
                 (progn (setq b 1) T)
                 (progn (setq c 1) T))
            (list a b c)`, result: new List([
        new Int(1), new Int(0), new Int(0)
    ])},
    {test: `(setq a 0 b 0 c 0)
            (and (progn (setq a 1) T)
                 (progn (setq b 1) nil)
                 (progn (setq c 1) T))
            (list a b c)`, result: new List([
        new Int(1), new Int(1), new Int(0)
    ])},
    {test: `(setq a 0 b 0 c 0)
            (and (progn (setq a 1) T)
                 (progn (setq b 1) T)
                 (progn (setq c 1) nil))
            (list a b c)`, result: new List([
        new Int(1), new Int(1), new Int(1)
    ])},
    {test: `(setq a 0 b 0 c 0)
            (and (progn (setq a 1) T)
                 (progn (setq b 1) T)
                 (progn (setq c 1) T))
            (list a b c)`, result: new List([
        new Int(1), new Int(1), new Int(1)
    ])},
]

QUnit.test('and', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

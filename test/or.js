import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Int, List} from '../src/VeLispTypes.js'

const tests = [
    {test: '(or)', result: new Bool(false)},
    {test: '(Or)', result: new Bool(false)},
    {test: '(OR)', result: new Bool(false)},

    {test: '(or nil)', result: new Bool(false)},
    {test: '(or T)', result: new Bool(true)},

    {test: '(or nil nil)', result: new Bool(false)},
    {test: '(or T T)', result: new Bool(true)},
    {test: '(or T nil)', result: new Bool(true)},
    {test: '(or nil T)', result: new Bool(true)},

    {test: '(or T nil nil)', result: new Bool(true)},
    {test: '(or nil T nil)', result: new Bool(true)},
    {test: '(or nil nil T)', result: new Bool(true)},
    {test: '(or nil nil nil)', result: new Bool(false)},
    {test: '(or T T T)', result: new Bool(true)},

    // Short circuit
    {test: `(setq a 0 b 0 c 0)
            (or (progn (setq a 1) nil)
                (progn (setq b 1) T)
                (progn (setq c 1) T))
            (list a b c)`, result: new List([
        new Int(1), new Int(1), new Int(0)
    ])},
    {test: `(setq a 0 b 0 c 0)
            (or (progn (setq a 1) nil)
                (progn (setq b 1) nil)
                (progn (setq c 1) T))
            (list a b c)`, result: new List([
        new Int(1), new Int(1), new Int(1)
    ])},
    {test: `(setq a 0 b 0 c 0)
            (or (progn (setq a 1) nil)
                (progn (setq b 1) nil)
                (progn (setq c 1) nil))
            (list a b c)`, result: new List([
        new Int(1), new Int(1), new Int(1)
    ])},
    {test: `(setq a 0 b 0 c 0)
            (or (progn (setq a 1) T)
                (progn (setq b 1) nil)
                (progn (setq c 1) nil))
            (list a b c)`, result: new List([
        new Int(1), new Int(0), new Int(0)
    ])},
]

QUnit.test('or', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

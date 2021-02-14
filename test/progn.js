const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(progn)', result: new Bool(false)},
    {test: '(Progn)', result: new Bool(false)},
    {test: '(PRogn)', result: new Bool(false)},
    {test: '(PROgn)', result: new Bool(false)},
    {test: '(PROGn)', result: new Bool(false)},
    {test: '(PROGN)', result: new Bool(false)},

    {test: '(progn 1)', result: new Int(1)},
    {test: '(progn 1 2 (+ 1 2))', result: new Int(3)},
    {test: `(setq a 0)
            (progn
              (setq a (- a 1))
              (setq a (- a 1)))`, result: new Int(-2)},
]

QUnit.test('progn', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})


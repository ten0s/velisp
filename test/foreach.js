const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(foreach n (list 1 2 3))', result: new Bool(false)},
    {test: '(Foreach n (list 1 2 3))', result: new Bool(false)},
    {test: '(FOreach n (list 1 2 3))', result: new Bool(false)},
    {test: '(FOReach n (list 1 2 3))', result: new Bool(false)},
    {test: '(FOREach n (list 1 2 3))', result: new Bool(false)},    
    {test: '(FOREAch n (list 1 2 3))', result: new Bool(false)},
    {test: '(FOREACh n (list 1 2 3))', result: new Bool(false)},
    {test: '(FOREACH n (list 1 2 3))', result: new Bool(false)},

    {test: '(foreach n (list 1 2 3) n)', result: new Int(3)},
    {test: '(foreach n (list 1 2 3) N)', result: new Int(3)},
    {test: '(foreach N (list 1 2 3) n)', result: new Int(3)},

    {test: `(setq fac 1)
            (foreach n (list 1 2 3 4 5)
              (setq fac (* n fac)))`, result: new Int(120)},
]

QUnit.test('foreach', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

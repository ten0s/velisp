const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Int, Pair} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(= 1)', result: new Bool(true)},
    {test: '(= 1.0)', result: new Bool(true)},
    {test: '(= "1.0")', result: new Bool(true)},

    {test: '(= 1 1))', result: new Bool(true)},
    {test: '(= 4 4.0)', result: new Bool(true)},
    {test: '(= 20 388)', result: new Bool(false)},
    {test: '(= 2.4 2.4 2.4)', result: new Bool(true)},
    {test: '(= 499 499 500)', result: new Bool(false)},

    {test: '(= "me" "me"))', result: new Bool(true)},
    {test: '(= "me" "you"))', result: new Bool(false)},

    // All args evaluated eagerly
    {test: `(setq a 0)
            (cons (= 1 2 (progn (setq a 1) 2)) a)`,
    result: new Pair(new Bool(false), new Int(1))},
]

const errors = [
    {test: '(=)', result: new Error('=: too few arguments')},
    {test: '(= nil)', result: new Error('=: expected Int, Real, Str')},
    {test: '(= 1 nil)', result: new Error('=: expected Int, Real, Str')},
]

QUnit.test('equalTo', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

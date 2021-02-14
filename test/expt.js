const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Int, Real} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(expt 2 4)', result: new Int(16)},
    {test: '(expt 2.0 4)', result: new Real(16.0)},
    {test: '(expt 2 4.0)', result: new Real(16.0)},
    {test: '(expt 3.0 2.0)', result: new Real(9.0)},
]

const errors = [
    {test: '(expt)', result: new Error('expt: too few arguments')},
    {test: '(expt 2)', result: new Error('expt: too few arguments')},    
    {test: '(expt 2 4 5)', result: new Error('expt: too many arguments')},
    {test: '(expt "2" 4)', result: new Error('expt: `num` expected Int, Real')},
    {test: '(expt 2 "4")', result: new Error('expt: `power` expected Int, Real')},
]

QUnit.test('expt', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

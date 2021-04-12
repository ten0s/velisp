const fs = require('fs')
const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(vl-file-size "d1")', result: new Int(0)},
    {test: '(vl-file-size "d2")', result: new Bool(false)},
    {test: `(setq f (open "f1" "w"))
            (princ "Hello" f)
            (close f)
            (vl-file-size "f1")`, result: new Int(5)},
]

const errors = [
    {test: '(vl-file-size)', result: new Error('vl-file-size: too few arguments')},
    {test: '(vl-file-size "f1" "f2")', result: new Error('vl-file-size: too many arguments')},
    {test: '(vl-file-size \'f1)', result: new Error('vl-file-size: expected Str')},
]
    
QUnit.test('vl-file-size', assert => {
    // Setup
    try { fs.mkdirSync('d1') } catch (e) { }

    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })

    // Tear down
    try {
        fs.unlinkSync('f1')
        fs.rmdirSync('d1')
    } catch (e) {}
})

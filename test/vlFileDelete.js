const fs = require('fs')
const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(setq f (open "f1" "w")) (close f) (vl-file-delete "f1")', result: new Bool(true)},
    {test: '(vl-file-delete "f2")', result: new Bool(false)},
]

const errors = [
    {test: '(vl-file-delete)', result: new Error('vl-file-delete: too few arguments')},
    {test: '(vl-file-delete "f1" "f2")', result: new Error('vl-file-delete: too many arguments')},
    {test: '(vl-file-delete \'f1)', result: new Error('vl-file-delete: expected Str')},
]
    
QUnit.test('vl-file-delete', assert => {
    // Setup
    // ...
    
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })

    // Tear down
    try { fs.unlinkSync('f1') } catch (e) {}
})

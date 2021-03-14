const fs = require('fs')
const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(vl-mkdir "t1")', result: new Bool(true)},
    {test: '(vl-mkdir "t2")', result: new Bool(false)},
    {test: '(vl-mkdir "t1/a/b")', result: new Bool(false)},
    {test: '(vl-mkdir "t1") (vl-mkdir "t1/a") (vl-mkdir "t1/a/b")', result: new Bool(true)},
]

const errors = [
    {test: '(vl-mkdir)', result: new Error('vl-mkdir: too few arguments')},
    {test: '(vl-mkdir "t1" "t2")', result: new Error('vl-mkdir: too many arguments')},
    {test: '(vl-mkdir \'t1)', result: new Error('vl-mkdir: expected Str')},
]
    
QUnit.test('vl-mkdir', assert => {
    // Setup
    try { fs.mkdirSync('t2') } catch (e) { }

    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })

    // Tear down
    try {
        fs.rmdirSync('t1/a/b')
        fs.rmdirSync('t1/a')
        fs.rmdirSync('t1')
        fs.rmdirSync('t2')
    } catch (e) {}
})

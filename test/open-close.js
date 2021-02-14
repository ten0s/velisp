const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(close (open "file.txt" "r"))', result: new Bool(false)},
    {test: '(close (open "file.txt" "w"))', result: new Bool(false)},
    {test: '(close (open "file.txt" "a"))', result: new Bool(false)},
]

const errors = [
    {test: '(open)', result: new Error('open: too few arguments')},
    {test: '(open "file.txt")', result: new Error('open: too few arguments')},
    {test: '(open "file.txt" "r" \'extra)', result: new Error('open: too many arguments')},

    // TODO
    //{test: '(open "unknown.txt" "r")', result: new Error()},
    
    {test: '(close)', result: new Error('close: too few arguments')},
    {test: '(close (open "file.txt" "w") \'extra)', result: new Error('close: too many arguments')},

    // TODO
    //{test: '(setq f (open "file.txt" "w")) (close f) (close f)', result: new Error()},
]

QUnit.test('open-close', assert => {
    const fs = require('fs')
    // Setup
    fs.closeSync(fs.openSync('file.txt', 'w'))

    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    // Tear down
    fs.unlinkSync('file.txt')

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

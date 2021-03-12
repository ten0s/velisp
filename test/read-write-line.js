const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Str} = require('../src/VeLispTypes.js')

const tests = [
    {test:`
(setq f (open "file.txt" "r"))
(setq res (read-line f))
(close f)
res
`, result: new Bool(false)},

    {test: `
(setq f (open "file.txt" "w"))
(write-line "Hello" f)
(write-line " " f)
(write-line "World" f)
(close f)
(setq f (open "file.txt" "r"))
(setq l1 (read-line f))
(setq l2 (read-line f))
(setq l3 (read-line f))
(close f)
(strcat l1 l2 l3)
`, result: new Str('Hello World')},
]

const errors = [
    {test: '(read-line)', result:
     new Error('read-line: too few arguments')},
    {test: '(setq f (open "file.txt" "w")) (read-line f f)', result:
     new Error('read-line: too many arguments')},
    {test: '(read-line 0)', result:
     new Error('read-line: `file-desc` expected File')},
    
    {test: '(write-line)', result:
     new Error('write-line: too few arguments')},
    {test: '(setq f (open "file.txt" "w")) (write-line "str" f f)', result:
     new Error('write-line: too many arguments')},
    {test: '(write-line 65 1)', result:
     new Error('write-line: `string` expected Str')},
    {test: '(write-line "str" 1)', result:
     new Error('write-line: `file-desc` expected File')},
]

QUnit.test('read-write-line', assert => {
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

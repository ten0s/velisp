const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Str} = require('../src/VeLispTypes.js')

const tests = [
    {test:`
(setq f (open "file.txt" "r"))
(setq res (read-char f))
(close f)
res
`, result: new Bool(false)},

    {test: `
(setq f (open "file.txt" "w"))
(write-char (ascii "H") f)
(write-char (ascii "e") f)
(write-char (ascii "l") f)
(write-char (ascii "l") f)
(write-char (ascii "o") f)
(close f)
(setq f (open "file.txt" "r"))
(setq c1 (chr (read-char f)))
(setq c2 (chr (read-char f)))
(setq c3 (chr (read-char f)))
(setq c4 (chr (read-char f)))
(setq c5 (chr (read-char f)))
(close f)
(strcat c1 c2 c3 c4 c5)
`, result: new Str('Hello')},
]

const errors = [
    {test: '(read-char)', result:
     new Error('read-char: too few arguments')},
    {test: '(setq f (open "file.txt" "r")) (read-char f f)', result:
     new Error('read-char: too many arguments')},
    {test: '(read-char 0)', result:
     new Error('read-char: `file-desc` expected File')},
    {test: '(setq f (open "file.txt" "r")) (close f) (read-char f)', result:
     new Error('read-char: bad file #<file "file.txt" r:c>')},
    {test: '(setq f (open "file.txt" "w")) (read-char f)', result:
     new Error('read-char: bad file #<file "file.txt" w:o>')},
    {test: '(setq f (open "file.txt" "a")) (read-char f)', result:
     new Error('read-char: bad file #<file "file.txt" a:o>')},
    
    {test: '(write-char)', result:
     new Error('write-char: too few arguments')},
    {test: '(setq f (open "file.txt" "w")) (write-char 65 f f)', result:
     new Error('write-char: too many arguments')},
    {test: '(write-char "A" 1)', result:
     new Error('write-char: `num` expected Int')},
    {test: '(write-char 65  1)', result:
     new Error('write-char: `file-desc` expected File')},
    {test: '(setq f (open "file.txt" "w")) (close f) (write-char 65 f)', result:
     new Error('write-char: bad file #<file "file.txt" w:c>')},
    {test: '(setq f (open "file.txt" "r")) (write-char 65 f)', result:
     new Error('write-char: bad file #<file "file.txt" r:o>')},
]

QUnit.test('read-write-char', assert => {
    const fs = require('fs')
    // Setup
    fs.closeSync(fs.openSync('file.txt', 'w'))

    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })

    // Tear down
    fs.unlinkSync('file.txt')
})

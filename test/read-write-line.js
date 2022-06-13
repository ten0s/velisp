import {TestRunner} from './test-runner.js'
import {rm, touch, tryRm, tryTouch} from './FsUtil.js'
import {Bool, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'read-write-line',

    setup: () => {
        touch('f1.txt')
        touch('f2.txt')
        tryTouch('f3.txt')
        touch('f4.txt')
        tryTouch('f5.txt')
        tryTouch('f6.txt')
        tryTouch('f7.txt')
        touch('f8.txt')
        touch('f9.txt')
    },

    teardown: () => {
        rm('f1.txt')
        rm('f2.txt')
        tryRm('f3.txt')
        rm('f4.txt')
        tryRm('f5.txt')
        tryRm('f6.txt')
        tryRm('f7.txt')
        rm('f8.txt')
        rm('f9.txt')
    },

    tests: [
        {test:`
(setq f (open "f1.txt" "r"))
(setq res (read-line f))
(close f)
res
`, result: new Bool(false)},

        {test: `
(setq f (open "f2.txt" "w"))
(write-line "Hello" f)
(write-line " " f)
(write-line "World" f)
(close f)
(setq f (open "f2.txt" "r"))
(setq l1 (read-line f))
(setq l2 (read-line f))
(setq l3 (read-line f))
(close f)
(strcat l1 l2 l3)
`, result: new Str('Hello World')},
    ],

    errors: [
        // DISABLED since it reads from stdin
        //{test: '(read-line)', result:
        // new Error('read-line: too few arguments')},

        // Doesn't close f3.txt
        {test: '(setq f (open "f3.txt" "r")) (read-line f f)', result:
         new Error('read-line: too many arguments')},

        {test: '(read-line 0)', result:
         new Error('read-line: `file-desc` expected File')},

        // Read from closed file
        {test: '(setq f (open "f4.txt" "r")) (close f) (read-line f)', result:
         new Error('read-line: bad file #<file "f4.txt" r:c>')},

        // Doesn't close f5.txt
        {test: '(setq f (open "f5.txt" "w")) (read-line f)', result:
         new Error('read-line: bad file #<file "f5.txt" w:o>')},

        // Doesn't close f6.txt
        {test: '(setq f (open "f6.txt" "a")) (read-line f)', result:
         new Error('read-line: bad file #<file "f6.txt" a:o>')},

        {test: '(write-line)', result:
         new Error('write-line: too few arguments')},

        // Doesn't close f7.txt
        {test: '(setq f (open "f7.txt" "w")) (write-line "str" f f)', result:
         new Error('write-line: too many arguments')},

        {test: '(write-line 65 1)', result:
         new Error('write-line: `string` expected Str')},

        {test: '(write-line "str" 1)', result:
         new Error('write-line: `file-desc` expected File')},

        {test: '(setq f (open "f8.txt" "w")) (close f) (write-line "str" f)', result:
         new Error('write-line: bad file #<file "f8.txt" w:c>')},

        {test: '(setq f (open "f9.txt" "r")) (write-line "str" f)', result:
         new Error('write-line: bad file #<file "f9.txt" r:o>')},
    ]
})

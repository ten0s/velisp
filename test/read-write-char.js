import fs from 'fs'
import {TestRunner} from './test-runner.js'
import {rm, touch, tryRm, tryTouch} from './FsUtil.js'
import {Bool, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'read-write-char',

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
(setq res (read-char f))
(close f)
res
`, result: new Bool(false)},

        {test: `
(setq f (open "f2.txt" "w"))
(write-char (ascii "H") f)
(write-char (ascii "e") f)
(write-char (ascii "l") f)
(write-char (ascii "l") f)
(write-char (ascii "o") f)
(close f)
(setq f (open "f2.txt" "r"))
(setq c1 (chr (read-char f)))
(setq c2 (chr (read-char f)))
(setq c3 (chr (read-char f)))
(setq c4 (chr (read-char f)))
(setq c5 (chr (read-char f)))
(close f)
(strcat c1 c2 c3 c4 c5)
`, result: new Str('Hello')},
    ],

    errors: [
        // DISABLED since it reads from stdin
        //{test: '(read-char)', result:
        // new Error('read-char: too few arguments')},

        // Doesn't close f3.txt
        {test: '(setq f (open "f3.txt" "r")) (read-char f f)', result:
         new Error('read-char: too many arguments')},

        {test: '(read-char 0)', result:
         new Error('read-char: `file-desc` expected File')},

        // Read from closed file
        {test: '(setq f (open "f4.txt" "r")) (close f) (read-char f)', result:
         new Error('read-char: bad file #<file "f4.txt" r:c>')},

        // Doesn't close f5.txt
        {test: '(setq f (open "f5.txt" "w")) (read-char f)', result:
         new Error('read-char: bad file #<file "f5.txt" w:o>')},

        // Doesn't close f6.txt
        {test: '(setq f (open "f6.txt" "a")) (read-char f)', result:
         new Error('read-char: bad file #<file "f6.txt" a:o>')},

        {test: '(write-char)', result:
         new Error('write-char: too few arguments')},

        // Doesn't close f7.txt
        {test: '(setq f (open "f7.txt" "w")) (write-char 65 f f)', result:
         new Error('write-char: too many arguments')},

        {test: '(write-char "A" 1)', result:
         new Error('write-char: `num` expected Int')},

        {test: '(write-char 65  1)', result:
         new Error('write-char: `file-desc` expected File')},

        {test: '(setq f (open "f8.txt" "w")) (close f) (write-char 65 f)', result:
         new Error('write-char: bad file #<file "f8.txt" w:c>')},

        {test: '(setq f (open "f9.txt" "r")) (write-char 65 f)', result:
         new Error('write-char: bad file #<file "f9.txt" r:o>')},
    ]
})

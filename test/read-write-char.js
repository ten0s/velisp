import {TestRunner} from './test-runner.js'
import {rm, touch, tryRm, tryTouch} from './FsUtil.js'
import {Bool, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'read-write-char',

    setup: () => {
        touch('rwc1.txt')
        touch('rwc2.txt')
        tryTouch('rwc3.txt')
        touch('rwc4.txt')
        tryTouch('rwc5.txt')
        tryTouch('rwc6.txt')
        tryTouch('rwc7.txt')
        touch('rwc8.txt')
        tryTouch('rwc9.txt')
    },

    teardown: () => {
        rm('rwc1.txt')
        rm('rwc2.txt')
        tryRm('rwc3.txt')
        rm('rwc4.txt')
        tryRm('rwc5.txt')
        tryRm('rwc6.txt')
        tryRm('rwc7.txt')
        rm('rwc8.txt')
        tryRm('rwc9.txt')
    },

    tests: [
        {test:`
(setq f (open "rwc1.txt" "r"))
(setq res (read-char f))
(close f)
res
`, result: new Bool(false)},

        {test: `
(setq f (open "rwc2.txt" "w"))
(write-char (ascii "H") f)
(write-char (ascii "e") f)
(write-char (ascii "l") f)
(write-char (ascii "l") f)
(write-char (ascii "o") f)
(close f)
(setq f (open "rwc2.txt" "r"))
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

        // Doesn't close file
        {test: '(setq f (open "rwc3.txt" "r")) (read-char f f)', result:
         new Error('read-char: too many arguments')},

        {test: '(read-char 0)', result:
         new Error('read-char: `file-desc` expected File')},

        // Read from closed file
        {test: '(setq f (open "rwc4.txt" "r")) (close f) (read-char f)', result:
         new Error('read-char: bad file #<file "rwc4.txt" r:c>')},

        // Doesn't close file
        {test: '(setq f (open "rwc5.txt" "w")) (read-char f)', result:
         new Error('read-char: bad file #<file "rwc5.txt" w:o>')},

        // Doesn't close file
        {test: '(setq f (open "rwc6.txt" "a")) (read-char f)', result:
         new Error('read-char: bad file #<file "rwc6.txt" a:o>')},

        {test: '(write-char)', result:
         new Error('write-char: too few arguments')},

        // Doesn't close file
        {test: '(setq f (open "rwc7.txt" "w")) (write-char 65 f f)', result:
         new Error('write-char: too many arguments')},

        {test: '(write-char "A" 1)', result:
         new Error('write-char: `num` expected Int')},

        {test: '(write-char 65  1)', result:
         new Error('write-char: `file-desc` expected File')},

        // Write to closed file
        {test: '(setq f (open "rwc8.txt" "w")) (close f) (write-char 65 f)', result:
         new Error('write-char: bad file #<file "rwc8.txt" w:c>')},

        // Doesn't close file
        {test: '(setq f (open "rwc9.txt" "r")) (write-char 65 f)', result:
         new Error('write-char: bad file #<file "rwc9.txt" r:o>')},
    ]
})

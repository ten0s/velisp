import {TestRunner} from './test-runner.js'
import {rm, touch, tryRm, tryTouch} from './FsUtil.js'
import {Bool, List, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'read-write-line',

    setup: () => {
        touch('rwl1.txt')
        touch('rwl2.txt')
        tryTouch('rwl3.txt')
        touch('rwl4.txt')
        tryTouch('rwl5.txt')
        tryTouch('rwl6.txt')
        tryTouch('rwl7.txt')
        touch('rwl8.txt')
        tryTouch('rwl9.txt')
        touch('rwl10.txt')
        touch('rwl11.txt')
        touch('rwl12.txt')
    },

    teardown: () => {
        rm('rwl1.txt')
        rm('rwl2.txt')
        tryRm('rwl3.txt')
        rm('rwl4.txt')
        tryRm('rwl5.txt')
        tryRm('rwl6.txt')
        tryRm('rwl7.txt')
        rm('rwl8.txt')
        tryRm('rwl9.txt')
        rm('rwl10.txt')
        rm('rwl11.txt')
        rm('rwl12.txt')
    },

    tests: [
        // <EOF>
        {test:`
(setq f (open "rwl1.txt" "r"))
(setq res (read-line f))
(close f)
(list res)
`, result: new List([new Bool(false)])},

        // Hello<CR><SPACE><CR>World<CR><EOF>
        {test: `
(setq f (open "rwl2.txt" "w"))
(write-line "Hello" f)
(write-line " " f)
(write-line "World" f)
(close f)
(setq f (open "rwl2.txt" "r"))
(setq l1 (read-line f))
(setq l2 (read-line f))
(setq l3 (read-line f))
(setq l4 (read-line f))
(close f)
(list l1 l2 l3 l4)
`, result: new List([new Str('Hello'), new Str(' '), new Str('World'), new Bool(false)])},

        // Hello<EOF>
        {test: `
(setq f (open "rwl10.txt" "w"))
(write-char (ascii "H") f)
(write-char (ascii "e") f)
(write-char (ascii "l") f)
(write-char (ascii "l") f)
(write-char (ascii "o") f)
(close f)
(setq f (open "rwl10.txt" "r"))
(setq l1 (read-line f))
(setq l2 (read-line f))
(close f)
(list l1 l2)
`, result: new List([new Str('Hello'), new Bool(false)])},

        // Hello<CR><EOF>
        {test: `
(setq f (open "rwl11.txt" "w"))
(write-char (ascii "H") f)
(write-char (ascii "e") f)
(write-char (ascii "l") f)
(write-char (ascii "l") f)
(write-char (ascii "o") f)
(write-line ""f)
(close f)
(setq f (open "rwl11.txt" "r"))
(setq l1 (read-line f))
(setq l2 (read-line f))
(close f)
(list l1 l2)
`, result: new List([new Str('Hello'), new Bool(false)])},

        // Hello<CR><CR>World<CR><EOF>
        {test: `
(setq f (open "rwl12.txt" "w"))
(write-line "Hello" f)
(write-line "" f)
(write-line "World" f)
(close f)
(setq f (open "rwl12.txt" "r"))
(setq l1 (read-line f))
(setq l2 (read-line f))
(setq l3 (read-line f))
(setq l4 (read-line f))
(close f)
(list l1 l2 l3 l4)
`, result: new List([new Str('Hello'), new Str(''), new Str('World'), new Bool(false)])},
    ],

    errors: [
        // DISABLED since it reads from stdin
        //{test: '(read-line)', result:
        // new Error('read-line: too few arguments')},

        // Doesn't close f3.txt
        {test: '(setq f (open "rwl3.txt" "r")) (read-line f f)', result:
         new Error('read-line: too many arguments')},

        {test: '(read-line 0)', result:
         new Error('read-line: `file-desc` expected File')},

        // Read from closed file
        {test: '(setq f (open "rwl4.txt" "r")) (close f) (read-line f)', result:
         new Error('read-line: bad file #<file "rwl4.txt" r:c>')},

        // Doesn't close file
        {test: '(setq f (open "rwl5.txt" "w")) (read-line f)', result:
         new Error('read-line: bad file #<file "rwl5.txt" w:o>')},

        // Doesn't close file
        {test: '(setq f (open "rwl6.txt" "a")) (read-line f)', result:
         new Error('read-line: bad file #<file "rwl6.txt" a:o>')},

        {test: '(write-line)', result:
         new Error('write-line: too few arguments')},

        // Doesn't close file
        {test: '(setq f (open "rwl7.txt" "w")) (write-line "str" f f)', result:
         new Error('write-line: too many arguments')},

        {test: '(write-line 65 1)', result:
         new Error('write-line: `string` expected Str')},

        {test: '(write-line "str" 1)', result:
         new Error('write-line: `file-desc` expected File')},

        // Write to closed file
        {test: '(setq f (open "rwl8.txt" "w")) (close f) (write-line "str" f)', result:
         new Error('write-line: bad file #<file "rwl8.txt" w:c>')},

        // Doesn't close file
        {test: '(setq f (open "rwl9.txt" "r")) (write-line "str" f)', result:
         new Error('write-line: bad file #<file "rwl9.txt" r:o>')},
    ]
})

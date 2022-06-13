import {TestRunner} from './test-runner.js'
import {rm, touch} from './FsUtil.js'
import {Bool, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'read-write-line',

    setup: () => {
        touch('file.txt')
    },

    teardown: () => {
        rm('file.txt')
    },

    tests: [
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
    ],

    errors: [
        // Disabled since it reads from stdin
        //{test: '(read-line)', result:
        // new Error('read-line: too few arguments')},
        {test: '(setq f (open "file.txt" "r")) (read-line f f)', result:
         new Error('read-line: too many arguments')},
        {test: '(read-line 0)', result:
         new Error('read-line: `file-desc` expected File')},
        {test: '(setq f (open "file.txt" "r")) (close f) (read-line f)', result:
         new Error('read-line: bad file #<file "file.txt" r:c>')},
        {test: '(setq f (open "file.txt" "w")) (read-line f)', result:
         new Error('read-line: bad file #<file "file.txt" w:o>')},
        {test: '(setq f (open "file.txt" "a")) (read-line f)', result:
         new Error('read-line: bad file #<file "file.txt" a:o>')},

        {test: '(write-line)', result:
         new Error('write-line: too few arguments')},
        {test: '(setq f (open "file.txt" "w")) (write-line "str" f f)', result:
         new Error('write-line: too many arguments')},
        {test: '(write-line 65 1)', result:
         new Error('write-line: `string` expected Str')},
        {test: '(write-line "str" 1)', result:
         new Error('write-line: `file-desc` expected File')},
        {test: '(setq f (open "file.txt" "w")) (close f) (write-line "str" f)', result:
         new Error('write-line: bad file #<file "file.txt" w:c>')},
        {test: '(setq f (open "file.txt" "r")) (write-line "str" f)', result:
         new Error('write-line: bad file #<file "file.txt" r:o>')},
    ]
})

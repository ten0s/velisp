import {TestRunner} from './test-runner.js'
import {rm, touch} from './FsUtil.js'
import {Bool, Sym} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'open-close',

    setup: () => {
        touch('oc1.txt')
        touch('oc2.txt')
        touch('oc3.txt')
        touch('oc4.txt')
        touch('oc5.txt')
        touch('oc6.txt')
        touch('oc7.txt')
        touch('oc8.txt')
        touch('oc9.txt')
        rm('unknown.txt')
    },

    teardown: () => {
        rm('oc1.txt')
        rm('oc2.txt')
        rm('oc3.txt')
        rm('oc4.txt')
        rm('oc5.txt')
        rm('oc6.txt')
        rm('oc7.txt')
        rm('oc8.txt')
        rm('oc9.txt')
        rm('oc10.txt')
        rm('unknown.txt')
    },

    tests: [
        // Open known file succeeds for reading, writing and appending
        {test: '(setq f (open "oc1.txt" "r")) (close f) (type f)', result: new Sym('file')},
        {test: '(setq f (open "oc2.txt" "w")) (close f) (type f)', result: new Sym('file')},
        {test: '(setq f (open "oc3.txt" "a")) (close f) (type f)', result: new Sym('file')},

        // Open unknown file fails for reading
        {test: '(open "unknown.txt" "r")', result: new Bool(false)},
        // Open unknown file succeeds for writing and appending
        {test: '(setq f (open "unknown.txt" "w")) (close f) (type f)', result: new Sym('file')},
        {test: '(setq f (open "unknown.txt" "a")) (close f) (type f)', result: new Sym('file')},

        // Close works for files open for reading, writing and appending
        {test: '(close (open "oc4.txt" "r"))', result: new Bool(false)},
        {test: '(close (open "oc5.txt" "w"))', result: new Bool(false)},
        {test: '(close (open "oc6.txt" "a"))', result: new Bool(false)},

        // Double close works
        {test: '(setq f (open "oc8.txt" "w")) (close f) (close f)', result: new Bool(false)},
    ],

    errors: [
        {test: '(open)', result: new Error('open: too few arguments')},
        {test: '(open "oc10.txt")', result: new Error('open: too few arguments')},
        {test: '(open "oc10.txt" "r" \'extra)', result: new Error('open: too many arguments')},

        {test: '(close)', result: new Error('close: too few arguments')},
        {test: '(close 1)', result: new Error('close: `file-desc` expected File')},
        {test: '(close (open "oc10.txt" "w") \'extra)', result: new Error('close: too many arguments')},
    ]
})

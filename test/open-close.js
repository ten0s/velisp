import {TestRunner} from './test-runner.js'
import {rm, touch} from './FsUtil.js'
import {Bool, Sym} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'open-close',

    setup: () => {
        touch('f1.txt')
        touch('f2.txt')
        touch('f3.txt')
        touch('f4.txt')
        touch('f5.txt')
        touch('f6.txt')
        touch('f7.txt')
        touch('f8.txt')
        touch('f9.txt')
        rm('unknown.txt')
    },

    teardown: () => {
        rm('f1.txt')
        rm('f2.txt')
        rm('f3.txt')
        rm('f4.txt')
        rm('f5.txt')
        rm('f6.txt')
        rm('f7.txt')
        rm('f8.txt')
        rm('f9.txt')
        rm('f10.txt')
        rm('unknown.txt')
    },

    tests: [
        // Open known file succeeds for reading, writing and appending
        {test: '(setq f (open "f1.txt" "r")) (close f) (type f)', result: new Sym('file')},
        {test: '(setq f (open "f2.txt" "w")) (close f) (type f)', result: new Sym('file')},
        {test: '(setq f (open "f3.txt" "a")) (close f) (type f)', result: new Sym('file')},

        // Open unknown file fails for reading
        {test: '(open "unknown.txt" "r")', result: new Bool(false)},
        // Open unknown file succeeds for writing and appending
        {test: '(setq f (open "unknown.txt" "w")) (close f) (type f)', result: new Sym('file')},
        {test: '(setq f (open "unknown.txt" "a")) (close f) (type f)', result: new Sym('file')},

        // Close works for files open for reading, writing and appending
        {test: '(close (open "f4.txt" "r"))', result: new Bool(false)},
        {test: '(close (open "f5.txt" "w"))', result: new Bool(false)},
        {test: '(close (open "f6.txt" "a"))', result: new Bool(false)},

        // Double close works
        {test: '(setq f (open "f8.txt" "w")) (close f) (close f)', result: new Bool(false)},
    ],

    errors: [
        {test: '(open)', result: new Error('open: too few arguments')},
        {test: '(open "f10.txt")', result: new Error('open: too few arguments')},
        {test: '(open "f10.txt" "r" \'extra)', result: new Error('open: too many arguments')},

        {test: '(close)', result: new Error('close: too few arguments')},
        {test: '(close 1)', result: new Error('close: `file-desc` expected File')},
        {test: '(close (open "f10.txt" "w") \'extra)', result: new Error('close: too many arguments')},
    ]
})

import path from 'path'
import {TestRunner} from './test-runner.js'
import {Bool, Int, Real, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'getvar',

    tests: [
        {test: '(getvar "cdate")', result: (act) => act instanceof Real},
        {test: '(getvar "CDATE")', result: (act) => act instanceof Real},
        {test: '(getvar \'CDATE)', result: (act) => act instanceof Real},

        {test: '(getvar "millisecs")', result: (act) => act instanceof Int},
        {test: '(getvar "MILLISECS")', result: (act) => act instanceof Int},
        {test: '(getvar \'MILLISECS)', result: (act) => act instanceof Int},

        {test: '(getvar "unknown")', result: new Bool(false)},
        {test: '(getvar "UNKNOWN")', result: new Bool(false)},
        {test: '(getvar \'UNKNOWN)', result: new Bool(false)},

        {test: '(getvar "velisp-version")', result: (act) => act instanceof Str},
        {test: '(getvar "velisp-file")', result: (act) => {
            return act instanceof Str &&
                path.basename(act.value()) === '__TEST__'
        }},
        {test: `(defun test-func ( / line)
                  (princ "Hello")
                  (setq line (getvar "velisp-line"))
                  (princ "End")
                  line)
                (test-func)`,
        result: (act) => {
            return act instanceof Int &&
                act.value() === 3
        }},
        {test: '(getvar "velisp-function")', result: new Bool(false)},
        {test: `(defun test-func ( / name)
                  (princ "Hello")
                  (setq name (getvar "velisp-function"))
                  (princ "End")
                  name)
                (test-func)`,
        result: (act) => {
            return act instanceof Str &&
                act.value() === 'TEST-FUNC'
        }
        },
    ],

    errors: [
        {test: '(getvar)', result: new Error('getvar: too few arguments')},
        {test: '(getvar "VAR1" "VAR2")', result: new Error('getvar: too many arguments')},
        {test: '(getvar 1)', result: new Error('getvar: expected Str, Sym')},
    ]
})

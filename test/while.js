import {TestRunner} from './test-runner.js'
import {Bool, Int, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'while',

    tests: [
        {test: '(while nil "done")', result: new Bool(false)},
        {test: '(While nil "done")', result: new Bool(false)},
        {test: '(WHile nil "done")', result: new Bool(false)},
        {test: '(WHIle nil "done")', result: new Bool(false)},
        {test: '(WHILe nil "done")', result: new Bool(false)},
        {test: '(WHILE nil "done")', result: new Bool(false)},

        {test: `(setq test 1)
            (while (<= test 10)
              (setq test (+ 1 test)))`, result: new Int(11)},
        {test: `(setq test 1)
            (while (<= test 10)
              (setq test (+ 1 test))
              "done")`, result: new Str('done')},
    ]
})

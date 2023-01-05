import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-consp',

    tests: [
        {test: '(vl-consp nil)', result: new Bool(false)},
        {test: '(vl-consp ())', result: new Bool(false)},
        {test: '(vl-consp t)', result: new Bool(false)},
        {test: '(vl-consp 1)', result: new Bool(false)},
        {test: '(vl-consp 1.0)', result: new Bool(false)},
        {test: '(vl-consp "abc")', result: new Bool(false)},
        {test: '(vl-consp \'abc)', result: new Bool(false)},

        {test: '(vl-consp \'())', result: new Bool(false)},
        {test: '(vl-consp \'(1))', result: new Bool(true)},
        {test: '(vl-consp \'(1 2))', result: new Bool(true)},
        {test: '(vl-consp \'(1 2 3))', result: new Bool(true)},
        {test: '(vl-consp \'(1 . 2))', result: new Bool(true)},
        {test: '(vl-consp \'(1 . nil))', result: new Bool(true)},
        {test: '(vl-consp \'(nil . nil))', result: new Bool(true)},
        {test: '(vl-consp \'(1 2 . 3))', result: new Bool(true)},
    ],

    errors: [
        {test: '(vl-consp)', result: new Error('vl-consp: too few arguments')},
        {test: '(vl-consp nil nil)', result: new Error('vl-consp: too many arguments')},
    ]
})

import {TestRunner} from './test-runner.js'
import {Bool, Int} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-list-length',

    tests: [
        {test: '(vl-list-length nil)', result: new Int(0)},
        {test: '(vl-list-length \'())', result: new Int(0)},

        {test: '(vl-list-length \'(1))', result: new Int(1)},
        {test: '(vl-list-length \'(1 2))', result: new Int(2)},
        {test: '(vl-list-length \'(1 . nil))', result: new Int(1)},

        {test: '(vl-list-length \'(1 . 2))', result: new Bool(false)},

        {test: '(vl-list-length \'(1 (2 . 3) 4))', result: new Int(3)},
    ],

    errors: [
        {test: '(vl-list-length)', result: new Error('vl-list-length: too few arguments')},
        {test: '(vl-list-length nil nil)', result: new Error('vl-list-length: too many arguments')},
        {test: '(vl-list-length T)', result: new Error('vl-list-length: expected List, Pair')},
        {test: '(vl-list-length "1")', result: new Error('vl-list-length: expected List, Pair')},
    ]
})

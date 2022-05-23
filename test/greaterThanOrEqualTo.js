import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'greaterThanOrEqualTo',

    tests: [
        {test: '(>= nil)', result: new Bool(true)},
        {test: '(>= T)', result: new Bool(true)},
        {test: '(>= 1)', result: new Bool(true)},
        {test: '(>= "")', result: new Bool(true)},
        {test: '(>= \'(1))', result: new Bool(true)},

        {test: '(>= 1 1))', result: new Bool(true)},
        {test: '(>= 1 1.0)', result: new Bool(true)},
        {test: '(>= 1.0 1)', result: new Bool(true)},
        {test: '(>= 1.0 1.0)', result: new Bool(true)},
        {test: '(>= 120 17)', result: new Bool(true)},
        {test: '(>= 3.5 1794)', result: new Bool(false)},
        {test: '(>= 77 4 2)', result: new Bool(true)},
        {test: '(>= 77 4 4)', result: new Bool(true)},
        {test: '(>= 77 4 9)', result: new Bool(false)},
        {test: '(>= "c" "b"))', result: new Bool(true)},
        {test: '(>= "c" "c"))', result: new Bool(true)},
    ],

    errors: [
        {test: '(>=)', result: new Error('>=: too few arguments')},
    ]
})

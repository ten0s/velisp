import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'lessThanOrEqualTo',

    tests: [
        {test: '(<= nil)', result: new Bool(true)},
        {test: '(<= 1)', result: new Bool(true)},

        {test: '(<= nil nil)', result: new Bool(true)},
        {test: '(<= nil ())', result: new Bool(true)},
        {test: '(<= nil \'())', result: new Bool(true)},
        {test: '(<= nil "")', result: new Bool(true)},

        {test: '(<= 1 1))', result: new Bool(true)},
        {test: '(<= 1 1.0)', result: new Bool(true)},
        {test: '(<= 1.0 1)', result: new Bool(true)},
        {test: '(<= 1.0 1.0)', result: new Bool(true)},
        {test: '(<= 120 17)', result: new Bool(false)},
        {test: '(<= 17 120)', result: new Bool(true)},
        {test: '(<= 3.5 1794)', result: new Bool(true)},
        {test: '(<= 2 4 77)', result: new Bool(true)},
        {test: '(<= 4 4 77)', result: new Bool(true)},
        {test: '(<= 2 9 9)', result: new Bool(true)},
        {test: '(<= 2 9 4 5)', result: new Bool(false)},
        {test: '(<= "b" "b"))', result: new Bool(true)},
        {test: '(<= "b" "c"))', result: new Bool(true)},
    ],

    errors: [
        {test: '(<=)', result: new Error('<=: too few arguments')},
    ]
})

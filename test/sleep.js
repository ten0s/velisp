import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'sleep',

    tests: [
        {test: '(sleep 0)', result: new Bool(false)},
    ],

    errors: [
        {test: '(sleep)', result: new Error('sleep: too few arguments')},
        {test: '(sleep 1 2)', result: new Error('sleep: too many arguments')},
        {test: '(sleep "1")', result: new Error('sleep: expected Int')},
        {test: '(sleep -1)', result: new Error('sleep: expected positive Int')},
    ]
})

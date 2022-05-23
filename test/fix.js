import {TestRunner} from './test-runner.js'
import {Int} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'fix',

    tests: [
        {test: '(fix 1)', result: new Int(Math.floor(1))},
        {test: '(fix 1.0)', result: new Int(Math.floor(1.0))},
        {test: '(fix 2.2)', result: new Int(Math.floor(2.2))},
        {test: '(fix -0.4)', result: new Int(Math.floor(-0.4))},
    ],

    errors: [
        {test: '(fix)', result: new Error('fix: too few arguments')},
        {test: '(fix 0 1)', result: new Error('fix: too many arguments')},
    ]
})

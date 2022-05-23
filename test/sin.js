import {TestRunner} from './test-runner.js'
import {Real} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'sin',

    tests: [
        {test: '(sin 1)', result: new Real(Math.sin(1))},
        {test: '(sin 1.0)', result: new Real(Math.sin(1.0))},
        {test: '(sin 0)', result: new Real(Math.sin(0))},
        {test: '(sin 0.0)', result: new Real(Math.sin(0.0))},
    ],

    errors: [
        {test: '(sin)', result: new Error('sin: too few arguments')},
        {test: '(sin 0 1)', result: new Error('sin: too many arguments')},
    ]
})

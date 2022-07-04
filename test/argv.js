import {TestRunner} from './test-runner.js'
import {Bool, List, Argv0} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'argv',

    tests: [
        {test: '(argv)', result: (act) => { return act instanceof List }},
        {test: '(argv 0)', result: (act) => { return act instanceof Argv0 }},
        {test: '(argv 1)', result: new Bool(false)},
    ],

    errors: [
        {test: '(argv 0 "1")', result: new Error('argv: too many arguments')},
        {test: '(argv 0.5)', result: new Error('argv: expected Int')},
        {test: '(argv -1)', result: new Error('argv: expected positive Int')},
    ]
})

import os from 'os'
import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'homedir',

    tests: [
        {test: '(homedir)', result: new Str(os.homedir())},
    ],

    errors: [
        {test: '(homedir \'foo)', result: new Error('homedir: too many arguments')},
    ]
})

import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'
import {homeDir} from '../src/VeSystem.js'

TestRunner.run({
    name: 'homedir',

    tests: [
        {test: '(homedir)', result: new Str(homeDir())},
    ],

    errors: [
        {test: '(homedir \'foo)', result: new Error('homedir: too many arguments')},
    ]
})

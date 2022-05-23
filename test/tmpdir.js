import os from 'os'
import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'tmpdir',

    tests: [
        {test: '(tmpdir)', result: new Str(os.tmpdir())},
    ],

    errors: [
        {test: '(tmpdir \'foo)', result: new Error('tmpdir: too many arguments')},
    ]
})

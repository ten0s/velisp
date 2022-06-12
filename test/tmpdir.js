import os from 'os'
import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'
import {tmpDir} from '../src/VeSystem.js'

TestRunner.run({
    name: 'tmpdir',

    setup: () => {
        const oldtmp = process.env['TMP']
        const oldtemp = process.env['TEMP']
        return {oldtmp, oldtemp}
    },

    teardown: ({oldtmp, oldtemp}) => {
        process.env['TMP'] = oldtmp
        process.env['TEMP'] = oldtemp
    },

    tests: [
        {test: '(setenv "TMP" ".") (setenv "TEMP" "") (tmpdir)', result: new Str('.')},
        {test: '(setenv "TMP" "") (setenv "TEMP" ".") (tmpdir)', result: new Str('.')},
        {test: '(tmpdir)', result: new Str(tmpDir())},
    ],

    errors: [
        {test: '(tmpdir \'foo)', result: new Error('tmpdir: too many arguments')},
    ]
})

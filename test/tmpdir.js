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
        if (oldtmp !== undefined) {
            process.env['TMP'] = oldtmp
        } else {
            delete process.env['TMP']
        }
        if (oldtemp !== undefined) {
            process.env['TEMP'] = oldtemp
        } else {
            delete process.env['TEMP']
        }
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

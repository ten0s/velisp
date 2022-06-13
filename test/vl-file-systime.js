import {TestRunner} from './test-runner.js'
import {rm, tryRmdir} from './FsUtil.js'
import {Bool, Int, List} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-file-systime',

    setup: () => {},

    teardown: () => {
        rm('f1')
        tryRmdir('d1')
    },

    tests: [
        {test: '(vl-mkdir "d1") (vl-file-systime "d1")', result: new Bool(false)},
        {test: '(vl-file-systime "d2")', result: new Bool(false)},
        {test: '(close (open "f1" "w")) (vl-file-systime "f1")', result: (act) => {
            //console.log(act)
            return act instanceof List
                && act.length() === 8
                && act.value().every(x => x instanceof Int)
        }},
        {test: '(vl-file-systime "f2")', result: new Bool(false)},
    ],

    errors: [
        {test: '(vl-file-systime)', result: new Error('vl-file-systime: too few arguments')},
        {test: '(vl-file-systime "f1" "f2")', result: new Error('vl-file-systime: too many arguments')},
        {test: '(vl-file-systime \'f1)', result: new Error('vl-file-systime: expected Str')},
    ]
})

import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'startapp',

    tests: [
        {test: '(startapp "unknown")', result: new Bool(false)},
    ],

    testsLinux: [
        {test: '(startapp "bash")', result: (act) => Number.isInteger(act.value())},
        {test: '(startapp "bash" "--help")', result: (act) => Number.isInteger(act.value())},
    ],

    testsWindows: [
        {test: '(startapp "cmd.exe")', result: (act) => Number.isInteger(act.value())},
        {test: '(startapp "cmd.exe" "/?")', result: (act) => Number.isInteger(act.value())},
    ],

    errors: [
        {test: '(startapp)', result: new Error('startapp: too few arguments')},
        {test: '(startapp "anything" "--arg" "")', result: new Error('startapp: too many arguments')},
        {test: '(startapp \'anything)', result: new Error('startapp: `cmd` expected Str')},
        {test: '(startapp "anything" \'opt)', result: new Error('startapp: `file` expected Str')},
    ]
})

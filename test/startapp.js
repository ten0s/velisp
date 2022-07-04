import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'startapp',

    tests: [
        {test: '(startapp (argv 0) "--version")', result: (act) => Number.isInteger(act.value())},
        {test: '(startapp "unknown")', result: new Bool(false)},
        {test: '(startapp "unknown" "--arg" "")', result: new Bool(false)},
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
        {test: '(startapp \'anything)', result: new Error('startapp: `cmd` expected Str, Argv0')},
        {test: '(startapp "anything" \'opt)', result: new Error('startapp: `arg` expected Str')},
    ]
})

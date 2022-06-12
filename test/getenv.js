import {TestRunner} from './test-runner.js'
import {Bool, Str} from '../src/VeLispTypes.js'
import {homeDir, tmpDir} from '../src/VeSystem.js'

TestRunner.run({
    name: 'getenv',

    tests: [
        {test: '(getenv "UNKNOWN")', result: new Bool(false)},
        {test: '(getenv "HOME")', result: new Str(homeDir())},
        {test: '(getenv "TMP")', result: new Str(tmpDir())},
        {test: '(getenv "TEMP")', result: new Str(tmpDir())},
        {test: '(setenv "EMPTY" "") (getenv "EMPTY")', result: new Str('')},
    ],

    testsLinux: [
        // Case-sensitive
        {test: '(setenv "NONEMPTY" "value") (getenv "NONEMPTY")', result: new Str('value')},
        {test: '(setenv "NONEMPTY" "value") (getenv "nonempty")', result: new Bool(false)},
    ],

    testsWindows: [
        // Case-insensitive
        {test: '(setenv "NONEMPTY" "value") (getenv "NONEMPTY")', result: new Str('value')},
        {test: '(setenv "NONEMPTY" "value") (getenv "nonempty")', result: new Str('value')},
    ],

    errors: [
        {test: '(getenv)', result: new Error('getenv: too few arguments')},
        {test: '(getenv "VAR1" "VAR2")', result: new Error('getenv: too many arguments')},
        {test: '(getenv \'VAR)', result: new Error('getenv: expected Str')},
    ]
})

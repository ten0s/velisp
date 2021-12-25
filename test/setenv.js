const {TestRunner} = require('./test-runner.js')
const {Str} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'setenv',

    tests: [
        {test: '(setenv "NAME" "value")', result: new Str('value')},
        {test: '(setenv "NAME" "")', result: new Str('')},
        {test: '(setenv "NAME" "value") (getenv "NAME")', result: new Str('value')},
    ],

    errors: [
        {test: '(setenv)', result: new Error('setenv: too few arguments')},
        {test: '(setenv "NAME")', result: new Error('setenv: too few arguments')},
        {test: '(setenv "NAME" "VALUE" "VALUE")', result: new Error('setenv: too many arguments')},
        {test: '(setenv \'NAME "VALUE")', result: new Error('setenv: `name` expected Str')},
        {test: '(setenv "NAME" \'VALUE)', result: new Error('setenv: `value` expected Str')},
    ]
})

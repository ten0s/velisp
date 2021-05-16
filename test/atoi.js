const {TestRunner} = require('./test-runner.js')
const {Int} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'atoi',

    tests: [
        {test: '(atoi "3")', result: new Int(3)},
        {test: '(atoi "-17")', result: new Int(-17)},
        {test: '(atoi "3.9")', result: new Int(3)},
        {test: '(atoi "")', result: new Int(0)},
        {test: '(atoi "abc")', result: new Int(0)},
    ],

    errors: [
        {test: '(atoi)', result: new Error('atoi: too few arguments')},
        {test: '(atoi "1" "2")', result: new Error('atoi: too many arguments')},
        {test: '(atoi 1)', result: new Error('atoi: expected Str')},
    ]
})

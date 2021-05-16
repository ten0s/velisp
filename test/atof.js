const {TestRunner} = require('./test-runner.js')
const {Real} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'atof',

    tests: [
        {test: '(atof "3.9")', result: new Real(3.9)},
        {test: '(atof "-17.0")', result: new Real(-17.0)},
        {test: '(atof "3")', result: new Real(3.0)},
        {test: '(atof "")', result: new Real(0.0)},
        {test: '(atof "abc")', result: new Real(0.0)},
    ],

    errors: [
        {test: '(atof)', result: new Error('atof: too few arguments')},
        {test: '(atof "1.0" "2.0")', result: new Error('atof: too many arguments')},
        {test: '(atof 1.0)', result: new Error('atof: expected Str')},
    ]
})

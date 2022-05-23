import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'ftoa',

    tests: [
        {test: '(ftoa 0.0)', result: new Str('0.0')},
        {test: '(ftoa 0)', result: new Str('0')},
        {test: '(ftoa 33)', result: new Str('33')},
        {test: '(ftoa 33.3)', result: new Str('33.3')},
        {test: '(ftoa -4.2)', result: new Str('-4.2')},
    ],

    errors: [
        {test: '(ftoa)', result: new Error('ftoa: too few arguments')},
        {test: '(ftoa 1.0 2.0)', result: new Error('ftoa: too many arguments')},
        {test: '(ftoa "1")', result: new Error('ftoa: expected Int, Real')},
    ]
})

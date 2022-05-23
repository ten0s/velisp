import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'itoa',

    tests: [
        {test: '(itoa 33)', result: new Str('33')},
        {test: '(itoa -17)', result: new Str('-17')},
    ],

    errors: [
        {test: '(itoa)', result: new Error('itoa: too few arguments')},
        {test: '(itoa 1 2)', result: new Error('itoa: too many arguments')},
        {test: '(itoa "1")', result: new Error('itoa: expected Int')},
    ]
})

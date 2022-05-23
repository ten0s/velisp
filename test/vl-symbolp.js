import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-symbolp',

    tests: [
        {test: '(vl-symbolp \'t)', result: new Bool(true)},
        {test: '(vl-symbolp \'nil)', result: new Bool(false)},
        {test: '(vl-symbolp \'foo)', result: new Bool(true)},
        {test: '(vl-symbolp t)', result: new Bool(true)},
        {test: '(vl-symbolp nil)', result: new Bool(false)},
        {test: '(vl-symbolp 1)', result: new Bool(false)},
        {test: '(vl-symbolp (list 1))', result: new Bool(false)},
    ],

    errors: [
        {test: '(vl-symbolp)', result: new Error('vl-symbolp: too few arguments')},
        {test: '(vl-symbolp \'t 1)', result: new Error('vl-symbolp: too many arguments')},
    ]
})

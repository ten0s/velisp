import {TestRunner} from './test-runner.js'
import {Sym, Fun} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'function',

    tests: [
        {test: '(function car)', result: new Sym('car')},
        {test: '(function (defun id (x) x))', result: new Sym('id')},
        {test: '(function (lambda (x) x))', result: (act) => {
            return act instanceof Fun
        }},
        {test: '(setq func (lambda (x) x)) (function func)', result: new Sym('func')},
    ],

    errors: [
        {test: '(function nil)', result: new Error('function: expected Sym, Fun')},
        {test: '(function T)', result: new Error('function: expected Sym, Fun')},
        {test: '(function 1)', result: new Error('function: expected Sym, Fun')},
    ]
})

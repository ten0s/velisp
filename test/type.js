import {TestRunner} from './test-runner.js'
import {Bool, Sym} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'type',

    tests: [
        {test: '(type nil)', result: new Bool(false)},
        {test: '(type ())', result: new Bool(false)},
        {test: '(type \'())', result: new Bool(false)},
        {test: '(type t)', result: new Sym('sym')},
        {test: '(type 1)', result: new Sym('int')},
        {test: '(type 1.0)', result: new Sym('real')},
        {test: '(type "1")', result: new Sym('str')},
        {test: '(type \'foo)', result: new Sym('sym')},
        {test: '(type \'(1 2 3))', result: new Sym('list')},
        {test: '(type \'(1 . 2))', result: new Sym('list')},
        {test: '(type +)', result: new Sym('subr')},
        {test: '(type (lambda (x) x))', result: new Sym('usubr')},
    ],

    errors: [
        {test: '(type)', result: new Error('type: too few arguments')},
        {test: '(type 1 2)', result: new Error('type: too many arguments')},
    ]
})

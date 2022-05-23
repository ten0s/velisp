import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-every',

    tests: [
        {test: '(vl-every \'= \'(1 2 3) \'(1 2 3))', result: new Bool(true)},
        {test: '(vl-every \'< \'(1 2 3) \'(1 2 3))', result: new Bool(false)},
        {test: '(vl-every (lambda (x) (= (rem x 2) 0)) \'(2 4 6))', result: new Bool(true)},
        {test: '(vl-every (lambda (x) (= (rem x 2) 0)) \'(2 5 6))', result: new Bool(false)},
        {test: '(vl-every (lambda (x) (= (rem x 2) 0)) nil)', result: new Bool(true)},
        {test: '(vl-every (lambda (x) (= (rem x 2) 0)) ())', result: new Bool(true)},
        {test: '(vl-every (lambda (x) (= (rem x 2) 0)) \'())', result: new Bool(true)},

        // Diff length
        {test: '(vl-every \'= \'(1 2 3) \'(1 2))', result: new Bool(true)},
        {test: '(vl-every \'= \'(1 2 3) \'(1))', result: new Bool(true)},
        {test: '(vl-every \'= \'(1 2 3) ())', result: new Bool(true)},
        {test: '(vl-every \'= \'() \'(1 2 3))', result: new Bool(true)},
        {test: '(vl-every \'= \'(1) \'(1 2 3))', result: new Bool(true)},
        {test: '(vl-every \'= \'(1 2) \'(1 2 3))', result: new Bool(true)},
    ],

    errors: [
        {test: '(vl-every)', result: new Error('vl-every: too few arguments')},
        {test: '(vl-every \'+)', result: new Error('vl-every: too few arguments')},
        {test: '(vl-every "+" \'())', result: new Error('vl-every: `predicate` no such function "+"')},
        {test: '(vl-every \'+ "str")', result: new Error('vl-every: `list` expected List')},
    ]
})

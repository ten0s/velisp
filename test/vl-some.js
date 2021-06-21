const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-some',

    tests: [
        {test: '(vl-some \'= \'(1 2 3) \'(1 2 3))', result: new Bool(true)},
        {test: '(vl-some \'< \'(1 2 3) \'(1 2 3))', result: new Bool(false)},
        {test: '(vl-some (lambda (x) (= (rem x 2) 0)) \'(2 4 6))', result: new Bool(true)},
        {test: '(vl-some (lambda (x) (= (rem x 2) 0)) \'(2 5 6))', result: new Bool(true)},
        {test: '(vl-some (lambda (x) (= (rem x 2) 0)) \'(1 3 5))', result: new Bool(false)},
        {test: '(vl-some (lambda (x) (= (rem x 2) 0)) nil)', result: new Bool(false)},
        {test: '(vl-some (lambda (x) (= (rem x 2) 0)) ())', result: new Bool(false)},
        {test: '(vl-some (lambda (x) (= (rem x 2) 0)) \'())', result: new Bool(false)},
        
        // Diff length
        {test: '(vl-some \'= \'(1 2 3) \'(1 2))', result: new Bool(true)},
        {test: '(vl-some \'= \'(1 2 3) \'(1))', result: new Bool(true)},
        {test: '(vl-some \'= \'(1 2 3) ())', result: new Bool(false)},
        {test: '(vl-some \'= \'() \'(1 2 3))', result: new Bool(false)},
        {test: '(vl-some \'= \'(1) \'(1 2 3))', result: new Bool(true)},
        {test: '(vl-some \'= \'(1 2) \'(1 2 3))', result: new Bool(true)},
    ],

    errors: [
        {test: '(vl-some)', result: new Error('vl-some: too few arguments')},
        {test: '(vl-some \'+)', result: new Error('vl-some: too few arguments')},
        {test: '(vl-some "+" \'())', result: new Error('vl-some: `predicate` no such function "+"')},
        {test: '(vl-some \'+ "str")', result: new Error('vl-some: `list` expected List')},
    ]
})

import {TestRunner} from './test-runner.js'
import {Bool, Int, Pair} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'equalTo',

    tests: [
        {test: '(= nil)', result: new Bool(true)},
        {test: '(= T)', result: new Bool(true)},
        {test: '(= 1)', result: new Bool(true)},
        {test: '(= 1.0)', result: new Bool(true)},
        {test: '(= "1.0")', result: new Bool(true)},

        {test: '(= 1 1))', result: new Bool(true)},
        {test: '(= 4 4.0)', result: new Bool(true)},
        {test: '(= 20 388)', result: new Bool(false)},
        {test: '(= 2.4 2.4 2.4)', result: new Bool(true)},
        {test: '(= 499 499 500)', result: new Bool(false)},

        {test: '(= "me" "me"))', result: new Bool(true)},
        {test: '(= "me" "you"))', result: new Bool(false)},

        {test: '(= 1 "1"))', result: new Bool(false)},
        {test: '(= "1" 1))', result: new Bool(false)},

        {test: '(= 1.0 "1.0"))', result: new Bool(false)},
        {test: '(= "1.0" 1.0))', result: new Bool(false)},

        {test: '(= 1 nil))', result: new Bool(false)},
        {test: '(= nil 1))', result: new Bool(false)},

        {test: '(= 1.0 nil))', result: new Bool(false)},
        {test: '(= nil 1.0))', result: new Bool(false)},

        {test: '(= "1" nil))', result: new Bool(false)},
        {test: '(= nil "1"))', result: new Bool(false)},

        {test: '(= 1 \'one))', result: new Bool(false)},
        {test: '(= \'one 1))', result: new Bool(false)},
        {test: '(= \'one \'one))', result: new Bool(true)},

        {test: '(= 1 \'(1)))', result: new Bool(false)},
        {test: '(= \'(1) 1))', result: new Bool(false)},

        {test: '(= \'(1) \'(1))', result: new Bool(false)},                 // like eq
        {test: '(= \'(1 . 2) \'(1 . 2)))', result: new Bool(false)},        // like eq
        {test: '(= (lambda () 1) (lambda () 1))', result: new Bool(false)}, // like eq

        // All args evaluated eagerly
        {test: `(setq a 0)
            (cons (= 1 2 (progn (setq a 1) 2)) a)`, result:
         new Pair(new Bool(false), new Int(1))},
    ],

    errors: [
        {test: '(=)', result: new Error('=: too few arguments')},
    ]
})

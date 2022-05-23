import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'equal',

    tests: [
        {test: '(equal nil nil)', result: new Bool(true)},
        {test: '(equal T T)', result: new Bool(true)},
        {test: '(equal nil T))', result: new Bool(false)},
        {test: '(equal T nil))', result: new Bool(false)},

        {test: '(equal 1 1))', result: new Bool(true)},
        {test: '(equal 1 1.0)', result: new Bool(true)},
        {test: '(equal 1.0 1)', result: new Bool(true)},
        {test: '(equal 1.0 1.0)', result: new Bool(true)},

        {test: '(equal "foo" "foo"))', result: new Bool(true)},

        {test: '(equal \'foo \'foo))', result: new Bool(true)},
        {test: '(equal \'foo \'FOO))', result: new Bool(true)},

        {test: '(equal (list) (list))', result: new Bool(true)},
        {test: '(equal (list) nil)', result: new Bool(true)},
        {test: '(equal nil (list))', result: new Bool(true)},
        {test: '(equal (list 1 2 3) (list 1 2 3))', result: new Bool(true)},
        {test: '(equal \'(1 . 2) \'(1 . 2)))', result: new Bool(true)},
        {test: '(equal (lambda () 1) (lambda () 1))', result: new Bool(true)},

        {test: '(equal (cons 1 \'a) (cons 1 \'a))', result: new Bool(true)},
        {test: '(equal (cons 1 \'a) (cons 2 \'a))', result: new Bool(false)},

        {test: `(setq f1 (list a b c) f2 (list a b c) f3 f2)
            (equal f1 f1)`, result: new Bool(true)},
        {test: `(setq f1 (list a b c) f2 (list a b c) f3 f2)
            (equal f1 f2)`, result: new Bool(true)},
        {test: `(setq f1 (list a b c) f2 (list a b c) f3 f2)
            (equal f1 f3)`, result: new Bool(true)},

        {test: '(equal + +)', result: new Bool(true)},
    ],

    errors: [
        {test: '(equal)', result: new Error('equal: too few arguments')},
        {test: '(equal 1 2 3)', result: new Error('equal: too many arguments')},
    ]
})

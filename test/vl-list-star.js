import {TestRunner} from './test-runner.js'
import {Bool, Int, Str, List, Pair} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-list*',

    tests: [
        {test: '(vl-list* nil)', result: new Bool(false)},
        {test: '(vl-list* ())', result: new Bool(false)},
        {test: '(vl-list* t)', result: new Bool(true)},
        {test: '(vl-list* 1)', result: new Int(1)},
        {test: '(vl-list* "abc")', result: new Str('abc')},

        {test: '(vl-list* 0 "text")', result: new Pair(new Int(0), new Str('text'))},
        {test: '(vl-list* 1 2 3)', result: new List([
            new Int(1), new Pair(new Int(2), new Int(3))
        ])},

        {test: '(vl-list* 1 2 \'(3 4))', result: new List([
            new Int(1), new Int(2), new Int(3), new Int(4)
        ])},
    ],

    errors: [
        {test: '(vl-list*)', result: new Error('vl-list*: too few arguments')},
    ]
})

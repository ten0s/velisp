import {TestRunner} from './test-runner.js'
import {Int, List} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'mapcar',

    tests: [
        {test: '(mapcar \'+ nil)', result: new List([])},
        {test: '(mapcar \'+ ())', result: new List([])},
        {test: '(mapcar \'+ \'())', result: new List([])},

        {test: '(mapcar \'(lambda (x) x) \'(1 2 3))', result: new List([
            new Int(1), new Int(2), new Int(3)
        ])},
        {test: '(mapcar \'+ \'(1 2 3))', result: new List([
            new Int(1), new Int(2), new Int(3)
        ])},
        {test: '(mapcar \'(lambda (x) (* x x)) \'(1 2 3))', result: new List([
            new Int(1), new Int(4), new Int(9)
        ])},

        {test: '(mapcar \'+ \'(1 2 3) \'(9 8 7))', result: new List([
            new Int(10), new Int(10), new Int(10)
        ])},
        {test: '(mapcar \'(lambda (x y) (expt x y)) \'(1 2 3) \'(3 3 3))', result: new List([
            new Int(1), new Int(8), new Int(27)
        ])},

        // Diff length
        {test: '(mapcar \'+ \'(1 2 3) \'(4 5 6 7))', result: new List([
            new Int(5), new Int(7), new Int(9)
        ])},
    ],

    errors: [
        {test: '(mapcar)', result: new Error('mapcar: too few arguments')},
        {test: '(mapcar \'+)', result: new Error('mapcar: too few arguments')},
        {test: '(mapcar "+")', result: new Error('mapcar: too few arguments')},
        {test: '(mapcar "+" \'())', result: new Error('mapcar: no such function "+"')},
        {test: '(mapcar \'+ "str")', result: new Error('mapcar: `list` expected List')},
    ]
})

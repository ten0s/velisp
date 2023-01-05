import {TestRunner} from './test-runner.js'
import {Bool, Int, List} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'append',

    tests: [
        {test: '(append)', result: new Bool(false)},
        {test: '(append nil)', result: new List([])},
        {test: '(append ())', result: new List([])},
        {test: '(append \'())', result: new List([])},
        {test: '(append \'(1))', result: new List([new Int(1)])},
        {test: '(append \'(1) \'(2 3))', result: new List([
            new Int(1), new Int(2), new Int(3)
        ])},
        {test: '(append \'(97) \'(96 44) \'(98) \'(96 44) \'(99))', result: new List([
            new Int(97), new Int(96), new Int(44), new Int(98),
            new Int(96), new Int(44), new Int(99)
        ])},
    ],

    errors: [
        {test: '(append 1)', result: new Error('append: expected List')},
        {test: '(append \'(1) 2)', result: new Error('append: expected List')},
        {test: '(append \'(1) \'(2 . 3))', result: new Error('append: expected List')},
        {test: '(append \'(1 . 2) \'(3))', result: new Error('append: expected List')},
    ]
})

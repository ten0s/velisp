import {TestRunner} from './test-runner.js'
import {Bool, Int, Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'repeat',

    tests: [
        {test: '(repeat 0)', result: new Bool(false)},
        {test: '(repeat 1)', result: new Bool(false)},
        {test: '(repeat 5)', result: new Bool(false)},

        {test: '(repeat 1 "done")', result: new Str('done')},
        {test: '(repeat 5 "done")', result: new Str('done')},
        {test: '(repeat 5 "do" "done")', result: new Str('done')},
        {test: `(setq a 10 b 100)
                (repeat 4
                    (setq a (+ a 10))
                    (setq b (+ b 100)))`, result: new Int(500)},
    ],

    errors: [
        {test: '(repeat -1)', result: new Error('repeat: `num` expected non-negative Int')},
        {test: '(repeat "")', result: new Error('repeat: `num` expected non-negative Int')},
        {test: '(repeat nil)', result: new Error('repeat: `num` expected non-negative Int')},
        {test: '(repeat \'abc)', result: new Error('repeat: `num` expected non-negative Int')},
    ]
})

import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Bool, Int, Str, Sym} from '../src/VeLispTypes.js'

const tests = [
    {test: '(load "test/fib.lsp")', result: new Int(55)},
    {test: '(load "test\\\\fib.lsp")', result: new Int(55)},
    // Add .lsp extension
    {test: '(load "test/fib")', result: new Int(55)},
    // Handle with nil
    {test: '(load "test/fib.bad" nil)', result: new Bool(false)},
    // Handle with 42
    {test: '(load "test/fib.bad" 42)', result: new Int(42)},
    // Not resolved symbol
    {test: '(load "test/fib.bad" \'handled)', result: new Sym('handled')},
    // Resolved symbol
    {test: '(load "test/fib.bad" (defun handle () "handled"))', result: new Str('handled')},
]

const errors = [
    {test: '(load)', result: new Error('load: too few arguments')},
    {test: '(load "filename" \'handled \'error)', result: new Error('load: too many arguments')},
    {test: '(load \'filename)', result: new Error('load: `filename` expected Str')},
    {test: '(load "test/fib.bad")',
        result: new Error('load: test/fib.bad: No such file or directory')},
]

QUnit.test('load', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

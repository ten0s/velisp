const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Str, Sym} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(load "examples/fib.lsp")', result: new Int(55)},
    // Add .lsp extension
    {test: '(load "examples/fib")', result: new Int(55)},
    // Handle with nil
    {test: '(load "examples/fib.bad" nil)', result: new Bool(false)},
    // Handle with 42
    {test: '(load "examples/fib.bad" 42)', result: new Int(42)},
    // Not resolved symbol
    {test: '(load "examples/fib.bad" \'handled)', result: new Sym('handled')},
    // Resolved symbol
    {test: '(load "examples/fib.bad" (defun handle () "handled"))', result: new Str('handled')},
];

const errors = [
    {test: '(load)', result: new Error('load: too few arguments')},
    {test: '(load "filename" \'handled \'error)', result: new Error('load: too many arguments')},
    {test: '(load \'filename)', result: new Error('load: filename must be Str')},
    {test: '(load "examples/fib.bad")',
     result: new Error("load: examples/fib.bad: No such file or directory")},
];

QUnit.test("load", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

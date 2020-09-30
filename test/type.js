const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Sym} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(type nil)', result: new Bool(false)},
    {test: '(type ())', result: new Bool(false)},
    {test: '(type t)', result: new Sym('sym')},
    {test: '(type 1)', result: new Sym('int')},
    {test: '(type 1.0)', result: new Sym('real')},
    {test: '(type "1")', result: new Sym('str')},
    {test: '(type \'foo)', result: new Sym('sym')},
    {test: '(type \'())', result: new Sym('list')},
    {test: '(type \'(1 2 3))', result: new Sym('list')},
    {test: '(type \'(1 . 2))', result: new Sym('list')},
    {test: '(type +)', result: new Sym('subr')},
    {test: '(type (lambda (x) x))', result: new Sym('subr')},
];

const errors = [
    {test: '(type)', result: new Error('type: too few arguments')},
    {test: '(type 1 2)', result: new Error('type: too many arguments')},
];

QUnit.test("type", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Real, Str, Sym, List, Pair} = require('../src/VeLispTypes.js');

const tests = [
    {test: 'nil', result: new Bool(false)},
    {test: 'Nil', result: new Bool(false)},
    {test: 'NIL', result: new Bool(false)},

    {test: 't', result: new Bool(true)},
    {test: 'T', result: new Bool(true)},

    {test: '2', result: new Int(2)},

    {test: '2.0', result: new Real(2.0)},

    {test: '"2.0"', result: new Str('2.0')},

    {test: '\'foo', result: new Sym('foo')},

    {test: '(list)', result: new List([])},
    {test: '(list 1 2 3)', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},

    {test: '(cons 1 \'a)', result: new Pair(new Int(1), new Sym('a'))},
];

QUnit.test("types", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

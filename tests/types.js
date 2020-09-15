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

    assert.equal((new Bool(false)).toString(), 'nil');
    assert.equal((new Bool(true)).toString(), 'T');
    assert.equal((new Int(2)).toString(), '2');
    assert.equal((new Real(2.0)).toString(), '2.0');
    assert.equal((new Str('2.0')).toString(), '"2.0"');
    assert.equal((new Sym('foo')).toString(), '\'foo');
    assert.equal((new List([])).toString(), '()');
    assert.equal((new List([new Int(1), new Real(1.0), new Str('2.0')])).toString(),
                 '(1 1.0 "2.0")');
    assert.equal((new Pair(new Int(1), new Sym('a'))).toString(), '(1 . \'a)');
});

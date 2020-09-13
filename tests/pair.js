const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Int, Sym, Pair} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(cons 1 \'a)', result: new Pair(new Int(1), new Sym('a'))},
    {test: '(car (cons 1 \'a))', result: new Int(1)},
    {test: '(cdr (cons 1 \'a))', result: new Sym('a')},
];

QUnit.test("pair", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Real, Str, List} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(list)', result: new List([])},
    {test: '(list 1 2 3)', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
    {test: '(list 1 "2" 3.0 (list 4))', result: new List([
        new Int(1), new Str('2'), new Real(3.0), new List([new Int(4)])
    ])},

    {test: '(listp (list \'a \'b \'b))', result: new Bool(true)},
    {test: '(listp \'a)', result: new Bool(false)},
    {test: '(listp 4.343)', result: new Bool(false)},
    {test: '(listp nil)', result: new Bool(true)},

    {test: '(cons 1 (list))', result: new List([new Int(1)])},
    {test: '(cons 1 nil)', result: new List([new Int(1)])},
    {test: '(cons 1 (list 2 3))', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
    {test: '(cons 1 (cons 2 (cons 3 nil)))', result: new List([
        new Int(1), new Int(2), new Int(3)
    ])},
    {test: '(cons (list 1) (list 2 3))', result: new List([
        new List([new Int(1)]), new Int(2), new Int(3)
    ])},

    {test: '(car (list 1 2 3))', result: new Int(1)},
    {test: '(cdr (list 1 2 3))', result: new List([
        new Int(2), new Int(3)
    ])},
];

QUnit.test("list", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

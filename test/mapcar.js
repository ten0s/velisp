const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Real, Str, List, Pair} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(mapcar \'+ nil)', result: new List([])},
    {test: '(mapcar \'+ ())', result: new List([])},
    {test: '(mapcar \'+ \'())', result: new List([])},

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
];

const errors = [
    {test: '(mapcar)', result: new Error('mapcar: too few arguments')},
    {test: '(mapcar \'+)', result: new Error('mapcar: too few arguments')},
    {test: '(mapcar "+")', result: new Error('mapcar: too few arguments')},
    {test: '(mapcar "+" \'())', result: new Error('mapcar: no such function "+"')},
    {test: '(mapcar \'+ "str")', result: new Error('mapcar: `list` expected List')},
];

QUnit.test("mapcar", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

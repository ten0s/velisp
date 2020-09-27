const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(vl-symbolp \'t)', result: new Bool(true)},
    {test: '(vl-symbolp \'nil)', result: new Bool(false)},
    {test: '(vl-symbolp \'foo)', result: new Bool(true)},
    {test: '(vl-symbolp t)', result: new Bool(false)},   // TODO: ?
    {test: '(vl-symbolp nil)', result: new Bool(false)},
    {test: '(vl-symbolp 1)', result: new Bool(false)},
    {test: '(vl-symbolp (list 1))', result: new Bool(false)},
];

const errors = [
    {test: '(vl-symbolp)', result: new Error('vl-symbolp: too few arguments')},
    {test: '(vl-symbolp \'t 1)', result: new Error('vl-symbolp: too many arguments')},
];
    
QUnit.test("vl-symbolp", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

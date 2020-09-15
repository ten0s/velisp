const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(cwd)', result: new Str(process.cwd())},
];

const errors = [
    {test: '(cwd \'foo)', result: new Error('cwd: too many arguments')},
];

QUnit.test("cwd", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

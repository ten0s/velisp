const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Int} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(strlen)', result: new Int(0)},
    {test: '(strlen "")', result: new Int(0)},
    {test: '(strlen "" "")', result: new Int(0)},
    {test: '(strlen "abcd")', result: new Int(4)},
    {test: '(strlen "one" "two" "four")', result: new Int(10)},
];

const errors = [
    {test: '(strlen \'one)', result: new Error('strlen: expected Str')},
    {test: '(strlen "one" \'two)', result: new Error('strlen: expected Str')},    
    {test: '(strlen "one" "two" \'four)', result: new Error('strlen: expected Str')},
    {test: '(strlen "one" \'two "four")', result: new Error('strlen: expected Str')},
];
    
QUnit.test("strlen", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

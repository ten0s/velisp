const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(cond)', result: new Bool(false)},
    {test: '(Cond)', result: new Bool(false)},
    {test: '(COnd)', result: new Bool(false)},
    {test: '(CONd)', result: new Bool(false)},
    {test: '(COND)', result: new Bool(false)},

    {test: '(cond (nil))', result: new Bool(false)},
    {test: '(cond (nil) (T))', result: new Bool(true)},
    {test: '(cond (nil) (T "one" "two" "three"))', result: new Str('three')},
    {test: '(cond (nil) (T "one" "two") (T "three"))', result: new Str('two')},
    {test: '(cond (nil "no"))', result: new Bool(false)},
    {test: '(cond (T "yes"))', result: new Str('yes')},
    {test: '(cond (nil "no") (T "yes"))', result: new Str('yes')},
    {test: '(cond ((= 0 1) "no") ((= 1 1) "yes"))', result: new Str('yes')},
];

QUnit.test("cond", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

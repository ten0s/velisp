const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(repeat 1)', result: new Bool(false)},
    {test: '(repeat 5)', result: new Bool(false)},
    {test: '(repeat 1 "done")', result: new Str('done')},
    {test: '(repeat 5 "done")', result: new Str('done')},
    {test: '(repeat 5 "do" "done")', result: new Str('done')},
    {test: `(setq a 10 b 100)
            (repeat 4 
              (setq a (+ a 10))
              (setq b (+ b 100)))`, result: new Int(500)},
];

QUnit.test("repeat", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

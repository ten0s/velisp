const QUnit = require('qunit');
const {evaluate} = require('../VeLispEvaluator.js');
const {Bool, Int, Str} = require('../VeLispTypes.js');

const tests = [
    {test: '(while nil "done")', result: new Bool(false)},
    {test: `(setq test 1)
            (while (<= test 10)
              (setq test (+ 1 test)))`, result: new Int(11)},
    {test: `(setq test 1)
            (while (<= test 10)
              (setq test (+ 1 test))
              "done")`, result: new Str('done')},
];

QUnit.test("while", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });
});

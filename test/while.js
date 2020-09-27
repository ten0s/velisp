const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Bool, Int, Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(while nil "done")', result: new Bool(false)},
    {test: '(While nil "done")', result: new Bool(false)},
    {test: '(WHile nil "done")', result: new Bool(false)},
    {test: '(WHIle nil "done")', result: new Bool(false)},
    {test: '(WHILe nil "done")', result: new Bool(false)},
    {test: '(WHILE nil "done")', result: new Bool(false)},

    // TODO
    {test: '(WHILE nil)', result: new Bool(false)},

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

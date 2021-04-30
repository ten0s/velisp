const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Real, Str} = require('../src/VeLispTypes.js')

const tests = [
    {test: 'T', result: new Bool(true)},
    {test: 'PI', result: new Real(Math.PI)},
    {test: 'EOL', result: new Str(require('os').EOL)},
]

QUnit.test('const', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

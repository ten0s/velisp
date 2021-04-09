const os = require('os')
const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Str} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(tmpdir)', result: new Str(os.tmpdir())},
]

const errors = [
    {test: '(tmpdir \'foo)', result: new Error('tmpdir: too many arguments')},
]

QUnit.test('tmpdir', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})

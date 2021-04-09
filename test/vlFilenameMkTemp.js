const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
//const {Str} = require('../src/VeLispTypes.js')

const tests = [
    //{test: '(vl-filename-mktemp)', result: new Str('')},
    //{test: '(vl-filename-mktemp nil nil nil)', result: new Str('')},
]

const errors = [
    {test: '(vl-filename-mktemp nil nil nil nil)', result:
     new Error('vl-filename-mktemp: too many arguments')},
]
    
QUnit.test('vl-filename-mktemp', assert => {
    // Setup
    // ...
    
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })

    // Tear down
    // ...
})

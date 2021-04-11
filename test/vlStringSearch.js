const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Bool, Int} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(vl-string-search "foo" "pfooyey on you")', result: new Int(1)},
    {test: '(vl-string-search "who" "pfooyey on you")', result: new Bool(false)},
    {test: '(vl-string-search "foo" "fooey-more-fooey" 1)', result: new Int(11)},
]

const errors = [
    {test: '(vl-string-search)', result:
     new Error('vl-string-search: too few arguments')},
    {test: '(vl-string-search "foo")', result:
     new Error('vl-string-search: too few arguments')},
    {test: '(vl-string-search "foo" "bar" 0 "baz")', result:
     new Error('vl-string-search: too many arguments')},

    {test: '(vl-string-search \'foo "bar")', result:
     new Error('vl-string-search: `pattern` expected Str')},
    {test: '(vl-string-search "foo" \'bar)', result:
     new Error('vl-string-search: `string` expected Str')},
    {test: '(vl-string-search "foo" "bar" "0")', result:
     new Error('vl-string-search: `start-pos` expected Int')},
    {test: '(vl-string-search "foo" "bar" -1)', result:
     new Error('vl-string-search: `start-pos` expected non-negative Int')},
]
    
QUnit.test('vl-string-search', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    })
})
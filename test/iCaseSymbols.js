const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')
const {Str} = require('../src/VeLispTypes.js')

const tests = [
    {test: '(defun foo () "foo") (foo)', result: new Str('foo')},
    {test: '(defun foo () "foo") (Foo)', result: new Str('foo')},
    {test: '(defun foo () "foo") (FOo)', result: new Str('foo')},
    {test: '(defun foo () "foo") (FOO)', result: new Str('foo')},

    {test: '(defun Foo () "foo") (foo)', result: new Str('foo')},
    {test: '(defun FOo () "foo") (foo)', result: new Str('foo')},
    {test: '(defun FOO () "foo") (foo)', result: new Str('foo')},

    {test: '(setq foo "foo") foo', result: new Str('foo')},
    {test: '(setq foo "foo") Foo', result: new Str('foo')},
    {test: '(setq foo "foo") FOo', result: new Str('foo')},
    {test: '(setq foo "foo") FOO', result: new Str('foo')},

    {test: '(setq Foo "foo") foo', result: new Str('foo')},
    {test: '(setq FOo "foo") foo', result: new Str('foo')},
    {test: '(setq FOO "foo") foo', result: new Str('foo')},

    {test: '(setq foo "foo" FOO "FOO") foo', result: new Str('FOO')},
]

QUnit.test('defun', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    })
})

const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Int, Str} = require('../src/VeLispTypes.js');

const tests = [
    {test: '(atoi "3")', result: new Int(3)},
    {test: '(atoi "-17")', result: new Int(-17)},
    {test: '(atoi "3.9")', result: new Int(3)},
];

const errors = [
    {test: '(atoi)', result: new Error('atoi: too few arguments')},
    {test: '(atoi "1" "2")', result: new Error('atoi: too many arguments')},
    {test: '(atoi 1)', result: new Error('atoi: expected Str')},
    {test: '(atoi "")', result: new Error('atoi: conversion impossible')},
    {test: '(atoi "abc")', result: new Error('atoi: conversion impossible')},
];

QUnit.test("atoi", assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

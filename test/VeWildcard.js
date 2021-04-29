const QUnit = require('qunit')
const VeWildcard = require('../src/VeWildcard.js')

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('')
    assert.ok(wc.test(''))
    assert.notOk(wc.test('a'))
    assert.notOk(wc.test('A'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('a')
    assert.ok(wc.test('a'))
    assert.notOk(wc.test('A'))
    assert.notOk(wc.test('aa'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('abc')
    assert.ok(wc.test('abc'))
    assert.notOk(wc.test('aBc'))
    assert.notOk(wc.test('aBC'))
    assert.notOk(wc.test('ABC'))
    assert.notOk(wc.test('abcd'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('?')
    assert.ok(wc.test('.'))
    assert.ok(wc.test(','))
    assert.ok(wc.test('A'))
    assert.ok(wc.test('a'))
    assert.ok(wc.test('0'))
    assert.ok(wc.test('9'))
    assert.notOk(wc.test('aa'))
    assert.notOk(wc.test('AA'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('??')
    assert.ok(wc.test('..'))
    assert.ok(wc.test(',;'))
    assert.ok(wc.test('aa'))
    assert.ok(wc.test('AA'))
    assert.ok(wc.test('ab'))
    assert.ok(wc.test('BA'))
    assert.ok(wc.test('01'))
    assert.notOk(wc.test('a'))
    assert.notOk(wc.test('A'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('a?')
    assert.ok(wc.test('aa'))
    assert.ok(wc.test('ab'))
    assert.ok(wc.test('aA'))
    assert.ok(wc.test('aB'))
    assert.notOk(wc.test('AA'))
    assert.notOk(wc.test('AB'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('a?c')
    assert.ok(wc.test('abc'))
    assert.ok(wc.test('aXc'))
    assert.notOk(wc.test('Abc'))
    assert.notOk(wc.test('abxC'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('*')
    assert.ok(wc.test(''))
    assert.ok(wc.test('.'))
    assert.ok(wc.test('..'))
    assert.ok(wc.test('a'))
    assert.ok(wc.test('abc'))
    assert.ok(wc.test('abcdefghijklmnopqrstuvwxyz'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('a?c*d')
    assert.ok(wc.test('abcd'))
    assert.ok(wc.test('aXcYd'))
    assert.ok(wc.test('axcYZ123d'))
    assert.ok(wc.test('a.c.....d'))
    assert.ok(wc.test('a?c********d'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('*.txt')
    assert.ok(wc.test('.txt'))
    assert.ok(wc.test('file.txt'))
    assert.notOk(wc.test('.TXT'))
    assert.notOk(wc.test('FILE.TXT'))
})
const QUnit = require('qunit')
const VeWildcard = require('../src/VeWildcard.js')

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('')
    assert.ok(wc.test(''))
    assert.notOk(wc.test('a'))
    assert.notOk(wc.test('A'))
})

QUnit.test('VeWildcard a', assert => {
    const wc = new VeWildcard('a')
    assert.ok(wc.test('a'))
    assert.notOk(wc.test('A'))
    assert.notOk(wc.test('aa'))
})

QUnit.test('VeWildcard abc', assert => {
    const wc = new VeWildcard('abc')
    assert.ok(wc.test('abc'))
    assert.notOk(wc.test('aBc'))
    assert.notOk(wc.test('aBC'))
    assert.notOk(wc.test('ABC'))
    assert.notOk(wc.test('abcd'))
})

QUnit.test('VeWildcard #', assert => {
    const wc = new VeWildcard('#')
    assert.ok(wc.test('0'))
    assert.ok(wc.test('5'))
    assert.ok(wc.test('9'))
    assert.notOk(wc.test('a'))
    assert.notOk(wc.test('z'))
    assert.notOk(wc.test('A'))
    assert.notOk(wc.test('Z'))
    assert.notOk(wc.test('.'))
    assert.notOk(wc.test('#'))
})

QUnit.test('VeWildcard ###', assert => {
    const wc = new VeWildcard('###')
    assert.ok(wc.test('012'))
    assert.ok(wc.test('345'))
    assert.ok(wc.test('678'))
    assert.ok(wc.test('910'))
    assert.notOk(wc.test('abc'))
    assert.notOk(wc.test('zyz'))
    assert.notOk(wc.test('.,;'))
    assert.notOk(wc.test('####'))
})

QUnit.test('VeWildcard @', assert => {
    const wc = new VeWildcard('@')
    assert.ok(wc.test('a'))
    assert.ok(wc.test('g'))
    assert.ok(wc.test('z'))
    assert.ok(wc.test('A'))
    assert.ok(wc.test('G'))
    assert.ok(wc.test('Z'))
    assert.notOk(wc.test('0'))
    assert.notOk(wc.test('9'))
    assert.notOk(wc.test('.'))
    assert.notOk(wc.test('\n'))
    assert.notOk(wc.test('@'))
})

QUnit.test('VeWildcard @@@', assert => {
    const wc = new VeWildcard('@@@')
    assert.ok(wc.test('abc'))
    assert.ok(wc.test('XYZ'))
    assert.ok(wc.test('dEf'))
    assert.ok(wc.test('QwE'))
})

QUnit.test('VeWildcard ?', assert => {
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

QUnit.test('VeWildcard ??', assert => {
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
    const wc = new VeWildcard('*')
    assert.ok(wc.test(''))
    assert.ok(wc.test('.'))
    assert.ok(wc.test('..'))
    assert.ok(wc.test('a'))
    assert.ok(wc.test('abc'))
    assert.ok(wc.test('abcdefghijklmnopqrstuvwxyz'))
})

QUnit.test('VeWildcard a?', assert => {
    const wc = new VeWildcard('a?')
    assert.ok(wc.test('aa'))
    assert.ok(wc.test('ab'))
    assert.ok(wc.test('aA'))
    assert.ok(wc.test('aB'))
    assert.notOk(wc.test('AA'))
    assert.notOk(wc.test('AB'))
})

QUnit.test('VeWildcard a?c', assert => {
    const wc = new VeWildcard('a?c')
    assert.ok(wc.test('abc'))
    assert.ok(wc.test('aXc'))
    assert.notOk(wc.test('Abc'))
    assert.notOk(wc.test('abxC'))
})

QUnit.test('VeWildcard', assert => {
    const wc = new VeWildcard('a?c*d')
    assert.ok(wc.test('abcd'))
    assert.ok(wc.test('aXcYd'))
    assert.ok(wc.test('axcYZ123d'))
    assert.ok(wc.test('a.c.....d'))
    assert.ok(wc.test('a?c********d'))
})

QUnit.test('VeWildcard *.txt', assert => {
    const wc = new VeWildcard('*.txt')
    assert.ok(wc.test('.txt'))
    assert.ok(wc.test('file.txt'))
    assert.notOk(wc.test('.TXT'))
    assert.notOk(wc.test('FILE.TXT'))
})

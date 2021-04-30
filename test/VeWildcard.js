const QUnit = require('qunit')
const VeWildcard = require('../src/VeWildcard.js')

const DIGITS = '0123456789'
const LOWER_ALPHAS = 'abcdefghijklmnopqrstuvwxyz'
const UPPER_ALPHAS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
const ALPHAS = LOWER_ALPHAS + UPPER_ALPHAS
const ALNUMS = DIGITS + ALPHAS
const NON_ALNUMS = ' `~!@#$%^&*?()_-+=.,:;\'"\/|[]{}<>'
const CTRLS = '\n\r\t'

// TODO \e \0 -> ''
// \a -> (a)

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
    assert.ok(Array.from(DIGITS).every(c => wc.test(c)))
    assert.notOk(Array.from(ALPHAS).some(c => wc.test(c)))
    assert.notOk(Array.from(NON_ALNUMS).some(c => wc.test(c)))
    assert.notOk(Array.from(CTRLS).some(c => wc.test(c)))
})

QUnit.test('VeWildcard ###', assert => {
    const wc = new VeWildcard('###')
    assert.ok(wc.test('059'))
})

QUnit.test('VeWildcard @', assert => {
    const wc = new VeWildcard('@')
    assert.ok(Array.from(ALPHAS).every(c => wc.test(c)))
    assert.notOk(Array.from(DIGITS).every(c => wc.test(c)))
    assert.notOk(Array.from(NON_ALNUMS).some(c => wc.test(c)))
    assert.notOk(Array.from(CTRLS).some(c => wc.test(c)))
})

QUnit.test('VeWildcard @@@', assert => {
    const wc = new VeWildcard('@@@')
    assert.ok(wc.test('aGz'))
})

QUnit.test('VeWildcard .', assert => {
    const wc = new VeWildcard('.')
    assert.ok(Array.from(NON_ALNUMS).every(c => wc.test(c)))
    assert.ok(Array.from(CTRLS).every(c => wc.test(c)))
    assert.notOk(Array.from(ALNUMS).every(c => wc.test(c)))
})

QUnit.test('VeWildcard ...', assert => {
    const wc = new VeWildcard('...')
    assert.ok(wc.test('.,;'))
})

QUnit.test('VeWildcard ?', assert => {
    const wc = new VeWildcard('?')
    assert.notOk(wc.test(''))
    assert.ok(Array.from(DIGITS).every(c => wc.test(c)))
    assert.ok(Array.from(ALPHAS).every(c => wc.test(c)))
    assert.ok(Array.from(NON_ALNUMS).every(c => wc.test(c)))
    assert.ok(Array.from(CTRLS).every(c => wc.test(c)))
})

QUnit.test('VeWildcard ??', assert => {
    const wc = new VeWildcard('??')
    assert.ok(wc.test('..'))
    assert.ok(wc.test(',;'))
    assert.ok(wc.test('a9'))
    assert.ok(wc.test('0Z'))
})

QUnit.test('VeWildcard *', assert => {
    const wc = new VeWildcard('*')
    assert.ok(wc.test(''))
    assert.ok(Array.from(DIGITS).every(c => wc.test(c)))
    assert.ok(Array.from(ALPHAS).every(c => wc.test(c)))
    assert.ok(Array.from(NON_ALNUMS).every(c => wc.test(c)))
    assert.ok(Array.from(CTRLS).every(c => wc.test(c)))
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

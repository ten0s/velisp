const QUnit = require('qunit')
require('../src/VeJsExt.js') // array.without
const VeWildcard = require('../src/VeWildcard.js')

const DIGITS = Array.from('0123456789')
const LOWER_ALPHAS = Array.from('abcdefghijklmnopqrstuvwxyz')
const UPPER_ALPHAS = Array.from('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
const ALPHAS = LOWER_ALPHAS.concat(UPPER_ALPHAS)
const ALNUMS = DIGITS.concat(ALPHAS)
const NON_ALNUMS = Array.from(' `~!@#$%^&*?()_-+=.,:;\'"\\/|[]{}<>')
const CTRLS = Array.from('\n\r\t\u001b')

// TODO \0 -> ''
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
    assert.ok(DIGITS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(ALPHAS.every(c => !wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard ###', assert => {
    const wc = new VeWildcard('###')
    assert.ok(wc.test('059'))
})

QUnit.test('VeWildcard @', assert => {
    const wc = new VeWildcard('@')
    assert.ok(ALPHAS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(DIGITS.every(c => !wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard @@@', assert => {
    const wc = new VeWildcard('@@@')
    assert.ok(wc.test('aGz'))
})

QUnit.test('VeWildcard .', assert => {
    const wc = new VeWildcard('.')
    assert.ok(NON_ALNUMS.every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(ALNUMS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard ...', assert => {
    const wc = new VeWildcard('...')
    assert.ok(wc.test('.,;'))
})

QUnit.test('VeWildcard ?', assert => {
    const wc = new VeWildcard('?')
    assert.notOk(wc.test(''))
    assert.ok(DIGITS.every(c => wc.test(c)))
    assert.ok(ALPHAS.every(c => wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
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
    assert.ok(DIGITS.every(c => wc.test(c)))
    assert.ok(ALPHAS.every(c => wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
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

QUnit.test('VeWildcard []', assert => {
    const wc = new VeWildcard('[]')
    assert.notOk(wc.test(''))
})

QUnit.test('VeWildcard [abc]', assert => {
    const wc = new VeWildcard('[abc]')
    assert.ok(wc.test('a'))
    assert.ok(wc.test('b'))
    assert.ok(wc.test('c'))
    // Negative cases
    assert.ok(ALPHAS.without(['a','b','c']).every(c => !wc.test(c)))
    assert.ok(DIGITS.every(c => !wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard [~abc]', assert => {
    const wc = new VeWildcard('[~abc]')
    assert.ok(ALPHAS.without(['a','b','c']).every(c => wc.test(c)))
    assert.ok(DIGITS.every(c => wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
    // Negative cases
    assert.notOk(wc.test('a'))
    assert.notOk(wc.test('b'))
    assert.notOk(wc.test('c'))
})

QUnit.test('VeWildcard [a-z]', assert => {
    const wc = new VeWildcard('[a-z]')
    assert.ok(LOWER_ALPHAS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(UPPER_ALPHAS.every(c => !wc.test(c)))
    assert.ok(DIGITS.every(c => !wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard [A-Z]', assert => {
    const wc = new VeWildcard('[A-Z]')
    assert.ok(UPPER_ALPHAS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(LOWER_ALPHAS.every(c => !wc.test(c)))
    assert.ok(DIGITS.every(c => !wc.test(c)))
    assert.ok(NON_ALNUMS.every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard [-ab]', assert => {
    const wc = new VeWildcard('[-ab]')
    assert.ok(wc.test('a'))
    assert.ok(wc.test('b'))
    assert.ok(wc.test('-'))
})

QUnit.test('VeWildcard [ab-]', assert => {
    const wc = new VeWildcard('[ab-]')
    assert.ok(wc.test('a'))
    assert.ok(wc.test('b'))
    assert.ok(wc.test('-'))
})

QUnit.test('VeWildcard [a~b]', assert => {
    const wc = new VeWildcard('[a~b]')
    assert.ok(wc.test('a'))
    assert.ok(wc.test('b'))
    assert.ok(wc.test('~'))
})

QUnit.test('VeWildcard [ab~]', assert => {
    const wc = new VeWildcard('[ab~]')
    assert.ok(wc.test('a'))
    assert.ok(wc.test('b'))
    assert.ok(wc.test('~'))
})

QUnit.test('VeWildcard [~-]', assert => {
    const wc = new VeWildcard('[~-]')
    assert.ok(ALPHAS.every(c => wc.test(c)))
    assert.ok(DIGITS.every(c => wc.test(c)))
    assert.ok(NON_ALNUMS.without(['-']).every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
})

QUnit.test('VeWildcard [`[]', assert => {
    const wc = new VeWildcard('[`[]')
    assert.ok(wc.test('['))
})

QUnit.test('VeWildcard [`]]', assert => {
    const wc = new VeWildcard('[`]]')
    assert.ok(wc.test(']'))
})

QUnit.test('VeWildcard [`[`]]+', assert => {
    const wc = new VeWildcard('[`[`]]+')
    assert.ok(wc.test('['))
    assert.ok(wc.test(']'))
    assert.ok(wc.test('[]'))
    assert.ok(wc.test(']['))
})

QUnit.test('VeWildcard [0-9a-zA-Z]', assert => {
    const wc = new VeWildcard('[0-9a-zA-Z]')
    assert.ok(ALPHAS.every(c => wc.test(c)))
    assert.ok(DIGITS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(NON_ALNUMS.every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

// Like above
QUnit.test('VeWildcard [0-9],[a-z],[A-Z]', assert => {
    const wc = new VeWildcard('[0-9],[a-z],[A-Z]')
    assert.ok(ALPHAS.every(c => wc.test(c)))
    assert.ok(DIGITS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(NON_ALNUMS.every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard [0-9,a-z,A-Z]', assert => {
    const wc = new VeWildcard('[0-9,a-z,A-Z]')
    assert.ok(ALPHAS.every(c => wc.test(c)))
    assert.ok(DIGITS.every(c => wc.test(c)))
    assert.ok(wc.test(','))
    // Negative cases
    assert.ok(NON_ALNUMS.without([',']).every(c => !wc.test(c)))
    assert.ok(CTRLS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard [~0-9a-zA-Z]', assert => {
    const wc = new VeWildcard('[~0-9a-zA-Z]')
    assert.ok(NON_ALNUMS.every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(ALPHAS.every(c => !wc.test(c)))
    assert.ok(DIGITS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard ~[0-9a-zA-Z]', assert => {
    const wc = new VeWildcard('~[0-9a-zA-Z]')
    assert.ok(NON_ALNUMS.every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(ALPHAS.every(c => !wc.test(c)))
    assert.ok(DIGITS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard ~[0-9],~[a-z],~[A-Z]', assert => {
    const wc = new VeWildcard('~[0-9a-zA-Z]')
    assert.ok(NON_ALNUMS.every(c => wc.test(c)))
    assert.ok(CTRLS.every(c => wc.test(c)))
    // Negative cases
    assert.ok(ALPHAS.every(c => !wc.test(c)))
    assert.ok(DIGITS.every(c => !wc.test(c)))
})

QUnit.test('VeWildcard ab`,cd,ef`,gh', assert => {
    const wc = new VeWildcard('ab`,cd,ef`,gh')
    assert.ok(wc.test('ab,cd'))
    assert.ok(wc.test('ef,gh'))
})

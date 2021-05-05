const QUnit = require('qunit')
const VeRegex = require('../src/VeRegex.js')

const DIGITS = '0123456789'
const LOWER_ALPHAS = 'abcdefghijklmnopqrstuvwxyz'
const UPPER_ALPHAS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
const ALPHAS = LOWER_ALPHAS + UPPER_ALPHAS
const ALNUMS = DIGITS + ALPHAS
const NON_ALNUMS = ' `~!@#$%^&*?()_-+=.,:;\'"\\/|[]{}<>'
const CTRLS = '\n\r\t\u001b'

// TODO \0 -> ''
// \a -> (a)

QUnit.test('VeRegex', assert => {
    const re = new VeRegex('')
    assert.ok(re.test(''))
    assert.notOk(re.test('a'))
    assert.notOk(re.test('A'))
})

QUnit.test('VeRegex a', assert => {
    const re = new VeRegex('a')
    assert.ok(re.test('a'))
    assert.notOk(re.test('A'))
    assert.notOk(re.test('aa'))
})

QUnit.test('VeRegex abc', assert => {
    const re = new VeRegex('abc')
    assert.ok(re.test('abc'))
    assert.notOk(re.test('aBc'))
    assert.notOk(re.test('aBC'))
    assert.notOk(re.test('ABC'))
    assert.notOk(re.test('abcd'))
})

/*
QUnit.test('VeRegex #', assert => {
    const re = new VeRegex('#')
    assert.ok(Array.from(DIGITS).every(c => re.test(c)))
    // Negative cases
    assert.ok(Array.from(ALPHAS).every(c => !re.test(c)))
    assert.ok(Array.from(NON_ALNUMS).every(c => !re.test(c)))
    assert.ok(Array.from(CTRLS).every(c => !re.test(c)))
})

QUnit.test('VeRegex ###', assert => {
    const re = new VeRegex('###')
    assert.ok(re.test('059'))
})

QUnit.test('VeRegex @', assert => {
    const re = new VeRegex('@')
    assert.ok(Array.from(ALPHAS).every(c => re.test(c)))
    // Negative cases
    assert.ok(Array.from(DIGITS).every(c => !re.test(c)))
    assert.ok(Array.from(NON_ALNUMS).every(c => !re.test(c)))
    assert.ok(Array.from(CTRLS).every(c => !re.test(c)))
})

QUnit.test('VeRegex @@@', assert => {
    const re = new VeRegex('@@@')
    assert.ok(re.test('aGz'))
})
*/

QUnit.test('VeRegex .', assert => {
    const re = new VeRegex('.')
    assert.ok(Array.from(NON_ALNUMS).every(c => re.test(c)))
    assert.ok(Array.from(CTRLS).every(c => re.test(c)))
    assert.ok(Array.from(ALNUMS).every(c => re.test(c)))
})

QUnit.test('VeRegex a.b', assert => {
    const re = new VeRegex('a.b')
    assert.ok(Array.from(NON_ALNUMS).every(c => re.test(`a${c}b`)))
    assert.ok(Array.from(CTRLS).every(c => re.test(`a${c}b`)))
    assert.ok(Array.from(ALNUMS).every(c => re.test(`a${c}b`)))
    assert.notOk(re.test('ab'))
    assert.notOk(re.test('abc'))
})

QUnit.test('VeRegex a*', assert => {
    const re = new VeRegex('a*')
    assert.ok(re.test(''))
    assert.ok(re.test('a'))
    assert.ok(re.test('aa'))
    assert.ok(re.test('aaaaa'))

    // TODO: re should be '^a*$' to fail
    assert.ok(re.test('a*'))
    
    assert.notOk(re.test('b'))
    assert.notOk(re.test('bb'))
    assert.notOk(re.test('bbbbb'))
})

QUnit.test('VeRegex a*b', assert => {
    const re = new VeRegex('a*b')
    assert.ok(re.test('b'))
    assert.ok(re.test('ab'))
    assert.ok(re.test('aab'))
    assert.ok(re.test('aaaaab'))

    // TODO: re should be '^a*b$' to fail
    assert.ok(re.test('a*b'))
    
    assert.notOk(re.test('bb'))
    assert.notOk(re.test('bbbbb'))
})

QUnit.test('VeRegex a+', assert => {
    const re = new VeRegex('a+')
    assert.ok(re.test('a'))
    assert.ok(re.test('aa'))
    assert.ok(re.test('aaaaa'))

    // TODO: re should be '^a+$' to fail
    assert.ok(re.test('a+'))
    
    assert.notOk(re.test(''))
    assert.notOk(re.test('b'))
    assert.notOk(re.test('bb'))
    assert.notOk(re.test('bbbbb'))
})

QUnit.test('VeRegex a+b', assert => {
    const re = new VeRegex('a+b')
    assert.ok(re.test('ab'))
    assert.ok(re.test('aab'))
    assert.ok(re.test('aaaaab'))

    // TODO: re should be '^a+b$' to fail
    assert.ok(re.test('a+b'))
    
    assert.notOk(re.test('b'))
    assert.notOk(re.test('bb'))
    assert.notOk(re.test('bbbbb'))
})

QUnit.test('VeRegex (a)', assert => {
    const re = new VeRegex('(a)')
    assert.ok(re.test('a'))
    assert.notOk(re.test('A'))
    assert.notOk(re.test('aa'))
})

QUnit.test('VeRegex ((AB)+)', assert => {
    const re = new VeRegex('((AB)+)')
    assert.ok(re.test('AB'))
    assert.ok(re.test('ABABAB'))
    assert.notOk(re.test(''))
    assert.notOk(re.test('BBBAAA'))
})

QUnit.test('VeRegex (A*|(A*BA**BA*)*)', assert => {
    const re = new VeRegex('(A*|(A*BA**BA*)*)')
    assert.ok(re.test('AAA'))
    assert.ok(re.test('BBAABB'))
    assert.ok(re.test('BABAAA'))
    assert.notOk(re.test('ABA'))
    assert.notOk(re.test('BBB'))
    assert.notOk(re.test('BABBAAA'))
})

QUnit.test('VeRegex (A(B|C|D)E)', assert => {
    const re = new VeRegex('(A(B|C|D)E)')
    assert.ok(re.test('ABE'))
    assert.ok(re.test('ACE'))
    assert.ok(re.test('ADE'))
    assert.notOk(re.test('AAE'))
    assert.notOk(re.test('AEE'))
})

QUnit.test('VeRegex a?', assert => {
    const re = new VeRegex('a?')
    assert.ok(re.test(''))
    assert.ok(re.test('a'))
    //assert.ok(re.test('aa')) ? TODO
})

QUnit.test('VeRegex (A(B|C|D)?E)', assert => {
    const re = new VeRegex('(A(B|C|D)?E)')
    assert.ok(re.test('ABE'))
    assert.ok(re.test('ACE'))
    assert.ok(re.test('ADE'))

    assert.ok(re.test('AE'))

    assert.notOk(re.test('AAE'))
    assert.notOk(re.test('AEE'))
})

QUnit.test('VeRegex xa?c', assert => {
    const re = new VeRegex('xa?c')
    assert.ok(re.test('xc'))
    assert.ok(re.test('xac'))
    assert.notOk(re.test('xabc'))
})

/*
QUnit.test('VeRegex', assert => {
    const re = new VeRegex('a?c*d')
    assert.ok(re.test('abcd'))
    assert.ok(re.test('aXcYd'))
    assert.ok(re.test('axcYZ123d'))
    assert.ok(re.test('a.c.....d'))
    assert.ok(re.test('a?c********d'))
})

QUnit.test('VeRegex *.txt', assert => {
    const re = new VeRegex('*.txt')
    assert.ok(re.test('.txt'))
    assert.ok(re.test('file.txt'))
    assert.notOk(re.test('.TXT'))
    assert.notOk(re.test('FILE.TXT'))
})
*/

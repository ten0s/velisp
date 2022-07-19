import QUnit from 'qunit'
import '../src/VeJsExt.js' // Array.without
import VeRegex from '../src/VeRegex.js'

const DIGITS = Array.from('0123456789')
const LOWER_ALPHAS = Array.from('abcdefghijklmnopqrstuvwxyz')
const UPPER_ALPHAS = Array.from('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
const ALPHAS = LOWER_ALPHAS.concat(UPPER_ALPHAS)
const ALNUMS = DIGITS.concat(ALPHAS)
const NON_ALNUMS = Array.from(' `~!@#$%^&*?()_-+=.,:;\'"\\/|[]{}<>')
const CTRLS = Array.from('\n\r\t\u001b')

// TODO \0 -> ''
// \a -> (a)

QUnit.test('VeRegex', assert => {
    const re = new VeRegex('')
    assert.ok(re.test(''))
    // Negative cases
    assert.notOk(re.test('a'))
    assert.notOk(re.test('A'))
})

QUnit.test('VeRegex a', assert => {
    const re = new VeRegex('a')
    assert.ok(re.test('a'))
    // Negative cases
    assert.notOk(re.test('A'))
    assert.notOk(re.test('aa'))
})

QUnit.test('VeRegex abc', assert => {
    const re = new VeRegex('abc')
    assert.ok(re.test('abc'))
    // Negative cases
    assert.notOk(re.test('aBc'))
    assert.notOk(re.test('aBC'))
    assert.notOk(re.test('ABC'))
    assert.notOk(re.test('abcd'))
})

QUnit.test('VeRegex []', assert => {
    const re = new VeRegex('[]')
    assert.ok(LOWER_ALPHAS.every(c => !re.test(c)))
    assert.ok(UPPER_ALPHAS.every(c => !re.test(c)))
    assert.ok(DIGITS.every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
    // Negative cases
    assert.notOk(re.test(''))
})

// Behaves like '.'
QUnit.test('VeRegex [^]', assert => {
    const re = new VeRegex('[^]')
    assert.ok(LOWER_ALPHAS.every(c => re.test(c)))
    assert.ok(UPPER_ALPHAS.every(c => re.test(c)))
    assert.ok(DIGITS.every(c => re.test(c)))
    assert.ok(NON_ALNUMS.every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    // Negative cases
    assert.notOk(re.test(''))
})

QUnit.test('VeRegex [^^]', assert => {
    const re = new VeRegex('[^^]')
    assert.ok(LOWER_ALPHAS.every(c => re.test(c)))
    assert.ok(UPPER_ALPHAS.every(c => re.test(c)))
    assert.ok(DIGITS.every(c => re.test(c)))
    assert.ok(NON_ALNUMS.without(['^']).every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    // Negative cases
    assert.notOk(re.test('^'))
})

QUnit.test('VeRegex [abcdefghijklmnopqrstuvwxyz]', assert => {
    const re = new VeRegex('[abcdefghijklmnopqrstuvwxyz]')
    assert.ok(LOWER_ALPHAS.every(c => re.test(c)))
    // Negative cases
    assert.ok(UPPER_ALPHAS.every(c => !re.test(c)))
    assert.ok(DIGITS.every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [0123456789]', assert => {
    const re = new VeRegex('[0123456789]')
    assert.ok(DIGITS.every(c => re.test(c)))
    // Negative cases
    assert.ok(ALPHAS.every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [a-z]', assert => {
    const re = new VeRegex('[a-z]')
    assert.ok(LOWER_ALPHAS.every(c => re.test(c)))
    // Negative cases
    assert.ok(UPPER_ALPHAS.every(c => !re.test(c)))
    assert.ok(DIGITS.every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [^a-z]', assert => {
    const re = new VeRegex('[^a-z]')
    assert.ok(UPPER_ALPHAS.every(c => re.test(c)))
    assert.ok(DIGITS.every(c => re.test(c)))
    assert.ok(NON_ALNUMS.every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    // Negative cases
    assert.ok(LOWER_ALPHAS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [a-zA-Z0-9]', assert => {
    const re = new VeRegex('[a-zA-Z0-9]')
    assert.ok(ALPHAS.every(c => re.test(c)))
    assert.ok(DIGITS.every(c => re.test(c)))
    // Negative cases
    assert.ok(NON_ALNUMS.every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [^a-zA-Z0-9]', assert => {
    const re = new VeRegex('[^a-zA-Z0-9]')
    assert.ok(NON_ALNUMS.every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    // Negative cases
    assert.ok(ALPHAS.every(c => !re.test(c)))
    assert.ok(DIGITS.every(c => !re.test(c)))
})

// Borders only
QUnit.test('VeRegex [9-0]', assert => {
    const re = new VeRegex('[9-0]')
    assert.ok(re.test('0'))
    assert.ok(re.test('9'))
    // Negative cases
    assert.ok(LOWER_ALPHAS.every(c => !re.test(c)))
    assert.ok(UPPER_ALPHAS.every(c => !re.test(c)))
    assert.ok(DIGITS.without(['0', '9']).every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [-]', assert => {
    const re = new VeRegex('[-]')
    assert.ok(re.test('-'))
    // Negative cases
    assert.ok(DIGITS.every(c => !re.test(c)))
    assert.ok(ALPHAS.every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.without(['-']).every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [^-]', assert => {
    const re = new VeRegex('[^-]')
    assert.ok(DIGITS.every(c => re.test(c)))
    assert.ok(ALPHAS.every(c => re.test(c)))
    assert.ok(NON_ALNUMS.without(['-']).every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    // Negative cases
    assert.notOk(re.test('-'))
})

QUnit.test('VeRegex [-abc]', assert => {
    const re = new VeRegex('[-abc]')
    assert.ok(re.test('-'))
    assert.ok(re.test('a'))
    assert.ok(re.test('b'))
    assert.ok(re.test('c'))
    // Negative cases
    assert.ok(DIGITS.every(c => !re.test(c)))
    assert.ok(ALPHAS.without(['a', 'b', 'c']).every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.without(['-']).every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [abc-]', assert => {
    const re = new VeRegex('[abc-]')
    assert.ok(re.test('-'))
    assert.ok(re.test('a'))
    assert.ok(re.test('b'))
    assert.ok(re.test('c'))
    // Negative cases
    assert.ok(DIGITS.every(c => !re.test(c)))
    assert.ok(ALPHAS.without(['a', 'b', 'c']).every(c => !re.test(c)))
    assert.ok(NON_ALNUMS.without(['-']).every(c => !re.test(c)))
    assert.ok(CTRLS.every(c => !re.test(c)))
})

QUnit.test('VeRegex [^-abc]', assert => {
    const re = new VeRegex('[^-abc]')
    assert.ok(DIGITS.every(c => re.test(c)))
    assert.ok(ALPHAS.without(['a', 'b', 'c']).every(c => re.test(c)))
    assert.ok(NON_ALNUMS.without(['-']).every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    // Negative cases
    assert.notOk(re.test('-'))
})

QUnit.test('VeRegex [^abc-]', assert => {
    const re = new VeRegex('[^abc-]')
    assert.ok(DIGITS.every(c => re.test(c)))
    assert.ok(ALPHAS.without(['a', 'b', 'c']).every(c => re.test(c)))
    assert.ok(NON_ALNUMS.without(['-']).every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    // Negative cases
    assert.notOk(re.test('-'))
})

QUnit.test('VeRegex [abc][def]', assert => {
    const re = new VeRegex('[abc][def]')
    assert.ok(re.test('ad'))
    assert.ok(re.test('ae'))
    assert.ok(re.test('af'))
    assert.ok(re.test('bd'))
    assert.ok(re.test('be'))
    assert.ok(re.test('bf'))
    assert.ok(re.test('cd'))
    assert.ok(re.test('ce'))
    assert.ok(re.test('cf'))
})

QUnit.test('VeRegex [abc]?', assert => {
    const re = new VeRegex('[abc]?')
    assert.ok(re.test(''))
    assert.ok(re.test('a'))
    assert.ok(re.test('b'))
    assert.ok(re.test('c'))
    // Negative cases
    assert.notOk(re.test('d'))
})

QUnit.test('VeRegex [abc]+', assert => {
    const re = new VeRegex('[abc]+')
    assert.ok(re.test('a'))
    assert.ok(re.test('b'))
    assert.ok(re.test('c'))

    assert.ok(re.test('aa'))
    assert.ok(re.test('ab'))
    assert.ok(re.test('ac'))

    assert.ok(re.test('aaa'))
    assert.ok(re.test('aab'))
    assert.ok(re.test('aac'))
})

QUnit.test('VeRegex [abc]*', assert => {
    const re = new VeRegex('[abc]*')
    assert.ok(re.test(''))

    assert.ok(re.test('a'))
    assert.ok(re.test('b'))
    assert.ok(re.test('c'))

    assert.ok(re.test('aa'))
    assert.ok(re.test('ab'))
    assert.ok(re.test('ac'))

    assert.ok(re.test('aaa'))
    assert.ok(re.test('aab'))
    assert.ok(re.test('aac'))
})

QUnit.test('VeRegex [^abc]', assert => {
    const re = new VeRegex('[^abc]')
    assert.ok(re.test('d'))
    assert.ok(re.test('A'))
    assert.ok(re.test('0'))
    assert.ok(re.test('.'))
    // Negative cases
    assert.notOk(re.test(''))
    assert.notOk(re.test('a'))
    assert.notOk(re.test('b'))
    assert.notOk(re.test('c'))
})

// [^abc]?
// [^abc]*
// [^abc]+

QUnit.test('VeRegex [\\\\[]', assert => {
    const wc = new VeRegex('[\\[]')
    assert.ok(wc.test('['))
})

QUnit.test('VeRegex [\\\\]]', assert => {
    const wc = new VeRegex('[\\]]')
    assert.ok(wc.test(']'))
})

QUnit.test('VeRegex [\\\\[\\\\]]+', assert => {
    const wc = new VeRegex('[\\[\\]]+')
    assert.ok(wc.test('['))
    assert.ok(wc.test(']'))
    assert.ok(wc.test('[]'))
    assert.ok(wc.test(']['))
})

QUnit.test('VeRegex .', assert => {
    const re = new VeRegex('.')
    assert.ok(NON_ALNUMS.every(c => re.test(c)))
    assert.ok(CTRLS.every(c => re.test(c)))
    assert.ok(ALNUMS.every(c => re.test(c)))
})

QUnit.test('VeRegex a.b', assert => {
    const re = new VeRegex('a.b')
    assert.ok(NON_ALNUMS.every(c => re.test(`a${c}b`)))
    assert.ok(CTRLS.every(c => re.test(`a${c}b`)))
    assert.ok(ALNUMS.every(c => re.test(`a${c}b`)))
    // Negative cases
    assert.notOk(re.test('ab'))
    assert.notOk(re.test('abc'))
})

QUnit.test('VeRegex a*', assert => {
    const re = new VeRegex('a*')
    assert.ok(re.test(''))
    assert.ok(re.test('a'))
    assert.ok(re.test('aa'))
    assert.ok(re.test('aaaaa'))
    // Negative cases
    assert.notOk(re.test('a*'))
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
    // Negative cases
    assert.notOk(re.test('a*b'))
    assert.notOk(re.test('bb'))
    assert.notOk(re.test('bbbbb'))
})

QUnit.test('VeRegex a+', assert => {
    const re = new VeRegex('a+')
    assert.ok(re.test('a'))
    assert.ok(re.test('aa'))
    assert.ok(re.test('aaaaa'))
    // Negative cases
    assert.notOk(re.test(''))
    assert.notOk(re.test('a+'))
    assert.notOk(re.test('b'))
    assert.notOk(re.test('bb'))
    assert.notOk(re.test('bbbbb'))
})

QUnit.test('VeRegex a+b', assert => {
    const re = new VeRegex('a+b')
    assert.ok(re.test('ab'))
    assert.ok(re.test('aab'))
    assert.ok(re.test('aaaaab'))
    // Negative cases
    assert.notOk(re.test('a+b'))
    assert.notOk(re.test('b'))
    assert.notOk(re.test('bb'))
    assert.notOk(re.test('bbbbb'))
})

QUnit.test('VeRegex (a)', assert => {
    const re = new VeRegex('(a)')
    assert.ok(re.test('a'))
    // Negative cases
    assert.notOk(re.test('A'))
    assert.notOk(re.test('aa'))
})

QUnit.test('VeRegex ((AB)+)', assert => {
    const re = new VeRegex('((AB)+)')
    assert.ok(re.test('AB'))
    assert.ok(re.test('ABABAB'))
    // Negative cases
    assert.notOk(re.test(''))
    assert.notOk(re.test('BBBAAA'))
})

QUnit.test('VeRegex (A*|(A*BA*BA*)*)', assert => {
    const re = new VeRegex('(A*|(A*BA*BA*)*)')
    assert.ok(re.test('AAA'))
    assert.ok(re.test('BBAABB'))
    assert.ok(re.test('BABAAA'))
    // Negative cases
    assert.notOk(re.test('ABA'))
    assert.notOk(re.test('BBB'))
    assert.notOk(re.test('BABBAAA'))
})

QUnit.test('VeRegex (A(B|C|D)E)', assert => {
    const re = new VeRegex('(A(B|C|D)E)')
    assert.ok(re.test('ABE'))
    assert.ok(re.test('ACE'))
    assert.ok(re.test('ADE'))
    // Negative cases
    assert.notOk(re.test('AAE'))
    assert.notOk(re.test('AEE'))
})

QUnit.test('VeRegex a?', assert => {
    const re = new VeRegex('a?')
    assert.ok(re.test(''))
    assert.ok(re.test('a'))
    // Negative cases
    assert.notOk(re.test('a?'))
    assert.notOk(re.test('aa'))
})

QUnit.test('VeRegex (A(B|C|D)?E)', assert => {
    const re = new VeRegex('(A(B|C|D)?E)')
    assert.ok(re.test('ABE'))
    assert.ok(re.test('ACE'))
    assert.ok(re.test('ADE'))
    assert.ok(re.test('AE'))
    // Negative cases
    assert.notOk(re.test('AAE'))
    assert.notOk(re.test('AEE'))
})

QUnit.test('VeRegex xa?c', assert => {
    const re = new VeRegex('xa?c')
    assert.ok(re.test('xc'))
    assert.ok(re.test('xac'))
    // Negative cases
    assert.notOk(re.test('xabc'))
})

QUnit.test('VeRegex \\r\\n\\t\\u001b', assert => {
    const re = new VeRegex('\r\n\t\u001b')
    assert.ok(re.test('\r\n\t\u001b'))
})

QUnit.test('VeRegex [\\r\\n\\t\\u001b]', assert => {
    const re = new VeRegex('[\r\n\t\u001b]')
    assert.ok(re.test('\r'))
    assert.ok(re.test('\n'))
    assert.ok(re.test('\t'))
    assert.ok(re.test('\u001b'))
})

QUnit.test('VeRegex \\**', assert => {
    const re = new VeRegex('\\**')
    assert.ok(re.test(''))
    assert.ok(re.test('*'))
    assert.ok(re.test('*****'))
})

QUnit.test('VeRegex \\.*', assert => {
    const re = new VeRegex('\\.*')
    assert.ok(re.test(''))
    assert.ok(re.test('.'))
    assert.ok(re.test('.....'))
})

QUnit.test('VeRegex \\.+', assert => {
    const re = new VeRegex('\\.+')
    assert.ok(re.test('.'))
    assert.ok(re.test('.....'))
    // Negative cases
    assert.notOk(re.test(''))
})

QUnit.test('VeRegex \\.?', assert => {
    const re = new VeRegex('\\.?')
    assert.ok(re.test(''))
    assert.ok(re.test('.'))
    // Negative cases
    assert.notOk(re.test('..'))
})

QUnit.test('VeRegex a?c*d', assert => {
    const re = new VeRegex('a?c*d')
    assert.ok(re.test('d'))

    assert.ok(re.test('ad'))
    assert.ok(re.test('acd'))
    assert.ok(re.test('accd'))
    assert.ok(re.test('acccd'))
    assert.ok(re.test('acccccd'))

    assert.ok(re.test('cd'))
    assert.ok(re.test('ccd'))
    assert.ok(re.test('cccd'))
    assert.ok(re.test('cccccd'))
})

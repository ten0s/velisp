const QUnit = require('qunit')
const VeGlob = require('../src/VeGlob.js')

QUnit.test('VeGlob', assert => {
    const glob = new VeGlob('')
    assert.ok(glob.test(''))
    assert.notOk(glob.test('a'))
    assert.notOk(glob.test('A'))
})

QUnit.test('VeGlob a', assert => {
    const glob = new VeGlob('a')
    assert.ok(glob.test('a'))
    assert.ok(glob.test('A'))
    assert.notOk(glob.test('aa'))
    assert.notOk(glob.test('AA'))
    assert.notOk(glob.test('ab'))
    assert.notOk(glob.test('AB'))
})

QUnit.test('VeGlob abc', assert => {
    const glob = new VeGlob('abc')
    assert.ok(glob.test('abc'))
    assert.ok(glob.test('aBc'))
    assert.ok(glob.test('aBC'))
    assert.ok(glob.test('ABC'))
    assert.notOk(glob.test('abcd'))
})

QUnit.test('VeGlob ?', assert => {
    const glob = new VeGlob('?')
    assert.ok(glob.test('.'))
    assert.ok(glob.test('A'))
    assert.ok(glob.test('a'))
    assert.ok(glob.test('A'))
    assert.notOk(glob.test('aa'))
    assert.notOk(glob.test('AA'))
})

QUnit.test('VeGlob ??', assert => {
    const glob = new VeGlob('??')
    assert.ok(glob.test('..'))
    assert.ok(glob.test('aa'))
    assert.ok(glob.test('AA'))
    assert.ok(glob.test('ab'))
    assert.ok(glob.test('BA'))
    assert.notOk(glob.test('a'))
    assert.notOk(glob.test('A'))
})

QUnit.test('VeGlob a?', assert => {
    const glob = new VeGlob('a?')
    assert.notOk(glob.test('a'))
    assert.notOk(glob.test('A'))
    assert.ok(glob.test('aa'))
    assert.ok(glob.test('AA'))
    assert.ok(glob.test('ab'))
    assert.ok(glob.test('AB'))
})

QUnit.test('VeGlob a?c', assert => {
    const glob = new VeGlob('a?c')
    assert.notOk(glob.test('a'))
    assert.notOk(glob.test('A'))
    assert.notOk(glob.test('aa'))
    assert.notOk(glob.test('AA'))
    assert.notOk(glob.test('ab'))
    assert.notOk(glob.test('AB'))
    assert.ok(glob.test('abc'))
    assert.ok(glob.test('ABC'))
    assert.ok(glob.test('aXc'))
    assert.ok(glob.test('AxC'))
})

QUnit.test('VeGlob *', assert => {
    const glob = new VeGlob('*')
    assert.ok(glob.test(''))
    assert.ok(glob.test('.'))
    assert.ok(glob.test('..'))
    assert.ok(glob.test('a'))
    assert.ok(glob.test('abc'))
    assert.ok(glob.test('abcdefghijklmnopqrstuvwxyz'))
})

QUnit.test('VeGlob a?c*d', assert => {
    const glob = new VeGlob('a?c*d')
    assert.ok(glob.test('abcd'))
    assert.ok(glob.test('ABCD'))
    assert.ok(glob.test('aXcYd'))
    assert.ok(glob.test('AxCYZ123D'))
    assert.ok(glob.test('A.C.....D'))
    assert.ok(glob.test('A?C********D'))
})

QUnit.test('VeGlob *.txt', assert => {
    const glob = new VeGlob('*.txt')
    assert.ok(glob.test('.txt'))
    assert.ok(glob.test('file.txt'))
    assert.ok(glob.test('.TXT'))
    assert.ok(glob.test('FILE.TXT'))
    assert.notOk(glob.test('file.pdf'))
    assert.notOk(glob.test('file.xls'))
    assert.notOk(glob.test('file_txt'))
})

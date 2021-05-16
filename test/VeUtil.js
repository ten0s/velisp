const QUnit = require('qunit')
const {
    find,
    escape,
    unescape,
    isRecoverableInput,
} = require('../src/VeUtil.js')

QUnit.test('VeUtil find', assert => {
    assert.ok(find(1, [1, 2, 3]))
    assert.ok(find(2, [1, 2, 3]))
    assert.ok(find(3, [1, 2, 3]))
    assert.notOk(find(0, [1, 2, 3]))
})

QUnit.test('VeUtil escape', assert => {
    assert.equal(escape(''), '')
    assert.equal(escape('\\'), '\\\\')
    assert.equal(escape('"'), '\\"')
    assert.equal(escape('\\r'), '\\\\r')
    assert.equal(escape('\\n'), '\\\\n')
    assert.equal(escape('\\t'), '\\\\t')
    assert.equal(escape('\\e'), '\\\\e')
    assert.equal(escape(' \\ " \\r \\n \\t \\e '), ' \\\\ \\" \\\\r \\\\n \\\\t \\\\e ')
})

QUnit.test('VeUtil unescape', assert => {
    assert.equal(unescape(''), '')

    assert.equal(unescape('\\\\'), '\\')

    assert.equal(unescape('\\"'), '"')
    assert.equal(unescape('\\\\"'), '\\"')
    
    assert.equal(unescape('\\r'), '\r')
    assert.equal(unescape('\\\\r'), '\\r')

    assert.equal(unescape('\\n'), '\n')
    assert.equal(unescape('\\\\n'), '\\n')

    assert.equal(unescape('\\t'), '\t')
    assert.equal(unescape('\\\\t'), '\\t')

    assert.equal(unescape('\\e'), '\u001b')
    assert.equal(unescape('\\\\e'), '\\e')

    assert.equal(unescape(' \\\\ \\" \\r \\n \\t \\e '), ' \\ " \r \n \t \u001b ')

    assert.equal(unescape('\\0'), '')
    assert.equal(unescape('\\\\0'), '\\0')
    assert.equal(unescape('abc\\0def'), 'abc')

    assert.equal(
        unescape('c:\\\\root\\\\to\\\\name\\\\entry.txt'),
        'c:\\root\\to\\name\\entry.txt'
    )
})

QUnit.test('VeUtil isRecoverableInput', assert => {
    assert.ok(isRecoverableInput('('))
    assert.ok(isRecoverableInput('(list'))
    assert.ok(isRecoverableInput('\'('))
    assert.ok(isRecoverableInput('(+ 1 2 3'))
    assert.ok(isRecoverableInput('("1 2 3"'))
    assert.ok(isRecoverableInput('("1" "2" "3"'))
    assert.ok(isRecoverableInput('("1 (2 3"'))
    assert.ok(isRecoverableInput('("1 (2) 3"'))
    assert.ok(isRecoverableInput('("1 2) 3"'))
    assert.ok(isRecoverableInput('()('))
    assert.ok(isRecoverableInput('(()'))
    assert.ok(isRecoverableInput('((()())'))

    assert.notOk(isRecoverableInput(')'))
    assert.notOk(isRecoverableInput(')('))
    assert.notOk(isRecoverableInput('(()))'))
})

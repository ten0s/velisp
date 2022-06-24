import QUnit from 'qunit'
import {
    find,
    escape,
    unescape,
    isRecoverableInput,
    ensureLspExt,
    ensureDclExt,
    makeUnixPath,
    makeWinPath,
} from '../src/VeUtil.js'

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

QUnit.test('VeUtil ensureLspExt', assert => {
    assert.equal(ensureLspExt(''), '.lsp')
    assert.equal(ensureLspExt('file'), 'file.lsp')
    assert.equal(ensureLspExt('file.lsp'), 'file.lsp')
    assert.equal(ensureLspExt('file.LSP'), 'file.LSP')
})

QUnit.test('VeUtil ensureDclExt', assert => {
    assert.equal(ensureDclExt(''), '.dcl')
    assert.equal(ensureDclExt('file'), 'file.dcl')
    assert.equal(ensureDclExt('file.dcl'), 'file.dcl')
    assert.equal(ensureDclExt('file.DCL'), 'file.DCL')
})

QUnit.test('VeUtil makeUnixPath', assert => {
    assert.equal(makeUnixPath(''), '')
    assert.equal(makeUnixPath('path/to'), 'path/to')
    assert.equal(makeUnixPath('path/to/here'), 'path/to/here')
    assert.equal(makeUnixPath('path\\to'), 'path/to')
    assert.equal(makeUnixPath('path\\to\\here'), 'path/to/here')
    assert.equal(makeUnixPath('path\\to/here'), 'path/to/here')
})

QUnit.test('VeUtil makeWinPath', assert => {
    assert.equal(makeWinPath(''), '')
    assert.equal(makeWinPath('path/to'), 'path\\to')
    assert.equal(makeWinPath('path/to/here'), 'path\\to\\here')
    assert.equal(makeWinPath('path\\to'), 'path\\to')
    assert.equal(makeWinPath('path\\to\\here'), 'path\\to\\here')
    assert.equal(makeWinPath('path\\to/here'), 'path\\to\\here')
})

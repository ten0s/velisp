import QUnit from 'qunit'
import VeArgv from '../src/VeArgv.js'

QUnit.test('VeArgv.options', assert => {
    assert.deepEqual(VeArgv.options(), [
        {option: '-e, --eval <expr>', default: undefined, help: 'evaluate script'},
        {option: '--no-dcl'         , default: true     , help: 'run without dcl'},
    ])
})

QUnit.test('VeArgv.initArgv', assert => {
    assert.deepEqual(VeArgv.initArgv([]), [])
    assert.deepEqual(
        VeArgv.initArgv(['node', 'src/main.js']),
        ['node', 'src/main.js']
    )
    assert.deepEqual(
        VeArgv.initArgv(['node', 'src/main.js', 'script.lsp', '1', 'two']),
        ['node', 'src/main.js', 'script.lsp', '1', 'two']
    )
    assert.deepEqual(
        VeArgv.initArgv(['node', 'src/main.js', '--no-dcl', 'script.lsp', '1', 'two']),
        ['node', 'src/main.js', '--no-dcl', 'script.lsp', '1', 'two']
    )
    assert.deepEqual(
        VeArgv.initArgv(['node', 'src/main.js', '--', '1', 'two']),
        ['node', 'src/main.js']
    )
    assert.deepEqual(
        VeArgv.initArgv(['node', 'src/main.js', '--no-dcl', '--', '1', 'two']),
        ['node', 'src/main.js', '--no-dcl']
    )
})

QUnit.test('VeArgv.lspArgv', assert => {
    assert.deepEqual(VeArgv.lspArgv([]), [])
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js']),
        []
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', 'script.lsp', '1', 'two']),
        ['script.lsp', '1', 'two']
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', '--no-dcl', 'script.lsp', '1', 'two']),
        ['script.lsp', '1', 'two']
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', '-e', '(ver)', '1', 'two']),
        ['1', 'two']
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', '-e=(ver)', '1', 'two']),
        ['1', 'two']
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', '--eval', '(ver)', '1', 'two']),
        ['1', 'two']
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', '--eval=(ver)', '1', 'two']),
        ['1', 'two']
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', '--', '1', 'two']),
        ['--', '1', 'two']
    )
    assert.deepEqual(
        VeArgv.lspArgv(['node', 'src/main.js', '--no-dcl', '--eval', '(ver)', '--', '1', 'two']),
        ['--', '1', 'two']
    )
})

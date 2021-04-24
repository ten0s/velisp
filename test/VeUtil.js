const QUnit = require('qunit')
const {find, isRecoverableInput} = require('../src/VeUtil.js')

QUnit.test('VeUtil find', assert => {
    assert.ok(find(1, [1, 2, 3]))
    assert.ok(find(2, [1, 2, 3]))
    assert.ok(find(3, [1, 2, 3]))
    assert.notOk(find(0, [1, 2, 3]))
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

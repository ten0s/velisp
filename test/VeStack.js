import QUnit from 'qunit'
import VeStack from '../src/VeStack.js'

QUnit.test('VeStack empty', assert => {
    const S = new VeStack()

    assert.equal(0, S.size())
    assert.ok(S.isEmpty())
    assert.throws(() => S.top(), new Error('Stack is empty'))
    assert.throws(() => S.pop(), new Error('Stack is empty'))
})

QUnit.test('VeStack size=1', assert => {
    const S = new VeStack()
    S.push(1)

    assert.equal(1, S.size())
    assert.notOk(S.isEmpty())

    assert.equal(1, S.top())
    assert.equal(1, S.top(0))
    assert.equal(1, S.size())
    assert.notOk(S.isEmpty())

    assert.throws(() => S.top(1), new Error('Stack is empty'))
    assert.throws(() => S.top(10), new Error('Stack is empty'))

    assert.equal(1, S.pop())
    assert.equal(0, S.size())
    assert.ok(S.isEmpty())
})

QUnit.test('VeStack size=3', assert => {
    const S = new VeStack()
    S.push(1)
    S.push(2)
    S.push(3)

    assert.equal(3, S.size())
    assert.notOk(S.isEmpty())

    assert.equal(3, S.top())
    assert.equal(3, S.top(0))
    assert.equal(2, S.top(1))
    assert.equal(1, S.top(2))
    assert.equal(3, S.size())
    assert.notOk(S.isEmpty())

    assert.throws(() => S.top(3), new Error('Stack is empty'))
    assert.throws(() => S.top(10), new Error('Stack is empty'))

    assert.equal(3, S.pop())
    assert.equal(2, S.size())
    assert.notOk(S.isEmpty())

    assert.equal(2, S.pop())
    assert.equal(1, S.size())
    assert.notOk(S.isEmpty())

    assert.equal(1, S.pop())
    assert.equal(0, S.size())
    assert.ok(S.isEmpty())
})

QUnit.test('VeStack empty fold', assert => {
    const S = new VeStack()
    assert.equal(0, S.fold((acc, x) => x + acc, 0))
})

QUnit.test('VeStack size=1 fold', assert => {
    const S = new VeStack()
    S.push(1)
    assert.equal(1, S.fold((acc, x) => x + acc, 0))
})

QUnit.test('VeStack size=3 fold', assert => {
    const S = new VeStack()
    S.push(1)
    S.push(2)
    S.push(3)
    assert.equal(6, S.fold((acc, x) => x + acc, 0))
    assert.equal('123', S.fold((acc, x) => x + acc, ''))
})

QUnit.test('VeStack empty unwind', assert => {
    const S = new VeStack()
    assert.throws(() => S.unwind(), new Error('Stack is empty'))
})

QUnit.test('VeStack size=1 unwind', assert => {
    const S = new VeStack()
    S.push(1)
    S.unwind()
    assert.equal(1, S.size())
    assert.equal(1, S.top())
})

QUnit.test('VeStack size=3 unwind', assert => {
    const S = new VeStack()
    S.push(1)
    S.push(2)
    S.push(3)
    S.unwind()
    assert.equal(1, S.size())
    assert.equal(1, S.top())
})

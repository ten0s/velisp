const QUnit = require('qunit')
const {find} = require('../src/VeUtil.js')

QUnit.test('VeUtil find', assert => {
    assert.ok(find(1, [1, 2, 3]))
    assert.ok(find(2, [1, 2, 3]))
    assert.ok(find(3, [1, 2, 3]))
    assert.notOk(find(0, [1, 2, 3]))
})

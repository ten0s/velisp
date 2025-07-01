import QUnit from 'qunit'
import '../src/VeJsExt.js'

QUnit.test('Array.without', assert => {
    assert.deepEqual([1, 2, 3].without(), [1, 2, 3])
    assert.deepEqual([1, 2, 3].without([]), [1, 2, 3])

    assert.deepEqual([1, 2, 1, 2, 1].without(1), [2, 2])
    assert.deepEqual([1, 2, 1, 2, 1].without([1]), [2, 2])

    assert.deepEqual([1, 2, 3, 4, 5].without([2, 4]), [1, 3, 5])
    assert.deepEqual([1, 2, 3, 4, 5].without([1, 5]), [2, 3, 4])
})

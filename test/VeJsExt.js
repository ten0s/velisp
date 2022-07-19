import QUnit from 'qunit'
import '../src/VeJsExt.js'

QUnit.test('Array.with', assert => {
    assert.deepEqual([1, 2, 3].with(), [])
    assert.deepEqual([1, 2, 3].with([]), [])
    
    assert.deepEqual([1, 2, 1, 2, 1].with(1), [1, 1, 1])
    assert.deepEqual([1, 2, 1, 2, 1].with([1]), [1, 1, 1])

    assert.deepEqual([1, 2, 3, 4, 5].with([2, 4]), [2, 4])
    assert.deepEqual([1, 2, 3, 4, 5].with([1, 5]), [1, 5])
})

QUnit.test('Array.without', assert => {
    assert.deepEqual([1, 2, 3].without(), [1, 2, 3])
    assert.deepEqual([1, 2, 3].without([]), [1, 2, 3])
    
    assert.deepEqual([1, 2, 1, 2, 1].without(1), [2, 2])
    assert.deepEqual([1, 2, 1, 2, 1].without([1]), [2, 2])

    assert.deepEqual([1, 2, 3, 4, 5].without([2, 4]), [1, 3, 5])
    assert.deepEqual([1, 2, 3, 4, 5].without([1, 5]), [2, 3, 4])
})

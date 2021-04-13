const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')

class TestRunner {
    static run({name = 'test-name', setup = () => {}, teardown = () => {}, tests = [], errors = []}) {
        QUnit.test(name, assert => {
            let env = undefined
            try {
                env = setup()
            } catch (e) {}
        
            tests.forEach(t => {
                const actual = evaluate(t.test)
                if (typeof(t.result) === 'function') {
                    assert.ok(t.result(actual, env))
                } else {
                    assert.deepEqual(actual, t.result, t.test)
                }
            })
            
            errors.forEach(t => {
                assert.throws(() => evaluate(t.test), t.result, t.test)
            })

            try {
                teardown(env)
            } catch (e) {}
        })
    }
}

module.exports.TestRunner = TestRunner

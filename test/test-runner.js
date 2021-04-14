const QUnit = require('qunit')
const {evaluate} = require('../src/VeLispEvaluator.js')

class TestRunner {
    static run({name = 'test-name', setup = () => {}, teardown = () => {}, tests = [], errors = []}) {
        if (tests.length || errors.length) {
            QUnit.test(name, assert => {
                tests.forEach(t => {
                    let env = undefined
                    try { env = setup() } catch (e) {}

                    const actual = evaluate(t.test)
                    if (typeof(t.result) === 'function') {
                        assert.ok(t.result(actual, env), t.test)
                    } else {
                        assert.deepEqual(actual, t.result, t.test)
                    }

                    try { teardown(env) } catch (e) {}
                })

                errors.forEach(t => {
                    let env = undefined
                    try { env = setup() } catch (e) {}

                    assert.throws(() => evaluate(t.test), t.result, t.test)

                    try { teardown(env) } catch (e) {}
                })
            })
        }
    }
}

module.exports.TestRunner = TestRunner

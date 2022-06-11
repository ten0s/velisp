import QUnit from 'qunit'
import {evaluate} from '../src/VeLispEvaluator.js'

class TestRunner {
    static run({
        name = 'test-name',

        setup = () => {},
        teardown = () => {},

        tests = [],
        testsLinux = [],
        testsWindows = [],
        testsMacOS = [],

        errors = [],
        errorsLinux = [],
        errorsWindows = [],
        errorsMacOS = [],

    }) {
        if (tests.length       ||
            testsLinux.length  ||
            testsWindows.length    ||
            testsMacOS.length    ||
            errors.length      ||
            errorsLinux.length ||
            errorsWindows.length   ||
            errorsMacOS.length) {
            QUnit.test(name, assert => {
                const platform = process.platform

                // Run generic tests
                tests.forEach(t => TestRunner.runTest(assert, setup, teardown, t))

                // Run platform specific tests
                switch (platform) {
                case 'linux':
                    testsLinux.forEach(t => TestRunner.runTest(assert, setup, teardown, t))
                    break
                case 'win32':
                    testsWindows.forEach(t => TestRunner.runTest(assert, setup, teardown, t))
                    break
                case 'darwin':
                    testsMacOS.forEach(t => TestRunner.runTest(assert, setup, teardown, t))
                    break
                default:
                    throw new Error(`Unknown platform: ${platform}`)
                }

                // Run generic tests
                errors.forEach(t => TestRunner.runError(assert, setup, teardown, t))

                // Run platform specific tests
                switch (platform) {
                case 'linux':
                    errorsLinux.forEach(t => TestRunner.runTest(assert, setup, teardown, t))
                    break
                case 'win32':
                    errorsWindows.forEach(t => TestRunner.runTest(assert, setup, teardown, t))
                    break
                case 'darwin':
                    errorsMacOS.forEach(t => TestRunner.runTest(assert, setup, teardown, t))
                    break
                default:
                    throw new Error(`Unknown platform: ${platform}`)
                }
            })
        }
    }

    static runTest(assert, setup, teardown, t) {
        let env = undefined
        try { env = setup() } catch (e) {}

        const actual = evaluate(t.test)
        if (typeof(t.result) === 'function') {
            assert.ok(t.result(actual, env), t.test)
        } else {
            assert.deepEqual(actual, t.result, t.test)
        }

        try { teardown(env) } catch (e) {}
    }

    static runError(assert, setup, teardown, t) {
        let env = undefined
        try { env = setup() } catch (e) {}

        assert.throws(() => evaluate(t.test), t.result, t.test)

        try { teardown(env) } catch (e) {}
    }
}

export {
    TestRunner,
}

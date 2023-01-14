import {TestRunner} from './test-runner.js'
import {Bool, Sym} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'trace',

    tests: [
        {test: '(trace)', result: new Bool(false)},
        {test: '(trace) (trace)', result: new Bool(false)},
        {test: '(trace cos)', result: new Sym('cos')},        // existing
        {test: '(trace \'cos)', result: new Sym('cos')},      // existing
        {test: '(trace unknown)', result: new Bool(false)},   // unknown
        {test: '(trace \'unknown)', result: new Bool(false)}, // unknown
        {test: '(trace cos cos)', result: new Sym('cos')},
        {test: '(trace cos sin exp)', result: new Sym('exp')},
        // See trace.{exp,lsp}
    ],

    errors: [
        {test: '(trace 1)', result: new Error('trace: `function` expected Sym, Fun')},
        {test: '(trace cos 1)', result: new Error('trace: `function` expected Sym, Fun')},
        {test: '(trace cos 1 sin)', result: new Error('trace: `function` expected Sym, Fun')},
    ]
})

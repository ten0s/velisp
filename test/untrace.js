import {TestRunner} from './test-runner.js'
import {Bool, Sym} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'untrace',

    tests: [
        {test: '(untrace)', result: new Bool(false)},
        {test: '(untrace) (untrace)', result: new Bool(false)},
        {test: '(untrace cos)', result: new Sym('cos')},        // existing
        {test: '(untrace \'cos)', result: new Sym('cos')},      // existing
        {test: '(untrace unknown)', result: new Bool(false)},   // unknown
        {test: '(untrace \'unknown)', result: new Bool(false)}, // unknown
        {test: '(untrace cos cos)', result: new Sym('cos')},
        {test: '(untrace cos sin exp)', result: new Sym('exp')},
        // See trace.{exp,lsp}
    ],

    errors: [
        {test: '(untrace 1)', result: new Error('untrace: `function` expected Sym, Fun')},
        {test: '(untrace cos 1)', result: new Error('untrace: `function` expected Sym, Fun')},
        {test: '(untrace cos 1 sin)', result: new Error('untrace: `function` expected Sym, Fun')},
    ]
})

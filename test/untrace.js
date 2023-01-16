import {TestRunner} from './test-runner.js'
import {evaluate} from '../src/VeLispEvaluator.js'
import {List, Sym} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'untrace',

    setup:    () => { evaluate('(untrace)') },
    tearDown: () => { evaluate('(untrace)') },

    tests: [
        {test: '(untrace)', result: new List([])},
        {test: '(untrace) (untrace)', result: new List([])},
        // Existing
        {test: '(untrace cos)', result: new List([])},
        {test: '(trace cos) (untrace cos)', result: new List([
            new Sym('cos')
        ])},
        {test: '(trace cos) (untrace \'cos)', result: new List([
            new Sym('cos')
        ])},
        {test: '(trace cos) (untrace cos cos)', result: new List([
            new Sym('cos')
        ])},
        {test: '(trace cos sin exp) (untrace)', result: new List([
            new Sym('cos'), new Sym('exp'), new Sym('sin')
        ])},
        {test: '(trace cos sin exp) (setq ret (untrace cos)) (untrace)', result: new List([
            new Sym('exp'), new Sym('sin')
        ])},
        {test: '(trace cos sin exp) (untrace exp cos sin)', result: new List([
            new Sym('cos'), new Sym('exp'), new Sym('sin')
        ])},
        // Non-existing
        {test: '(trace cos) (setq ret (untrace unknown)) (untrace) ret', result: new List([])},
        {test: '(untrace unknown)', result: new List([])},
        {test: '(untrace \'unknown)', result: new List([])},
        // See trace.{exp,lsp}
    ],

    errors: [
        {test: '(untrace 1)', result: new Error('untrace: `function` expected Sym, Fun')},
        {test: '(untrace cos 1)', result: new Error('untrace: `function` expected Sym, Fun')},
        {test: '(untrace cos 1 sin)', result: new Error('untrace: `function` expected Sym, Fun')},
    ]
})

import {TestRunner} from './test-runner.js'
import {evaluate} from '../src/VeLispEvaluator.js'
import {Sym, List} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'trace',

    setup:    () => { evaluate('(untrace)') },
    tearDown: () => { evaluate('(untrace)') },

    tests: [
        {test: '(trace)', result: new List([])},
        {test: '(trace) (trace)', result: new List([])},
        // Existing
        {test: '(setq ret (trace cos)) (untrace) ret', result: new List([
            new Sym('cos')
        ])},
        {test: '(setq ret (trace \'cos)) (untrace) ret', result: new List([
            new Sym('cos')
        ])},
        {test: '(setq ret (trace cos \'cos)) (untrace) ret', result: new List([
            new Sym('cos')
        ])},
        {test: '(setq ret (trace cos sin exp)) (untrace) ret', result: new List([
            new Sym('cos'), new Sym('exp'), new Sym('sin')
        ])},
        {test: '(trace cos exp) (setq ret (trace sin)) (untrace) ret', result: new List([
            new Sym('sin')
        ])},
        // Non-existing
        {test: '(setq ret (trace unknown)) (untrace) ret', result: new List([])},
        {test: '(setq ret (trace \'unknown)) (untrace) ret', result: new List([
            new Sym('unknown')
        ])},
        // See trace.{exp,lsp}
    ],

    errors: [
        {test: '(trace 1)', result: new Error('trace: `function` expected Sym, Fun')},
        {test: '(trace cos 1)', result: new Error('trace: `function` expected Sym, Fun')},
        {test: '(trace cos 1 sin)', result: new Error('trace: `function` expected Sym, Fun')},
    ]
})

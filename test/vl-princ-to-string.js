import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'

// TODO: Implement +/- 1/4 ~= \261 \274

TestRunner.run({
    name: 'vl-princ-to-string',

    tests: [
        {test: '(vl-princ-to-string "abc")', result: new Str('abc')},
        {test: '(vl-princ-to-string "/myutilities")', result: new Str('/myutilities')},
        {test: '(vl-princ-to-string \'my-var)', result: new Str('MY-VAR')},
        {test: '(vl-princ-to-string "\\r")', result: new Str('\r')},
        {test: '(vl-princ-to-string "\\n")', result: new Str('\n')},
        {test: '(vl-princ-to-string "\\t")', result: new Str('\t')},
        {test: '(vl-princ-to-string "\\e")', result: new Str('\u001b')},
        {test: `(setq str "The \\"allowable\\" tolerance is +/- 1/4\\"")
            (vl-princ-to-string str)`, result:
         new Str('The "allowable" tolerance is +/- 1/4"')},
    ],

    errors: [
        {test: '(vl-princ-to-string)', result:
         new Error('vl-princ-to-string: too few arguments')},
        {test: '(vl-princ-to-string "abc" "def")', result:
         new Error('vl-princ-to-string: too many arguments')},
    ]
})

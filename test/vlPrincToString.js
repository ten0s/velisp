const {TestRunner} = require('./test-runner.js')
const {Str} = require('../src/VeLispTypes.js')

// TODO: Implement +/- 1/4 ~= \261 \274

TestRunner.run({
    name: 'vl-princ-to-string',

    tests: [
        {test: '(vl-princ-to-string "abc")', result: new Str('abc')},
        {test: '(vl-princ-to-string "/myutilities")', result: new Str('/myutilities')},
        {test: '(vl-princ-to-string \'my-var)', result: new Str('MY-VAR')},
        {test: `(setq str "The \\"allowable\\" tolerance is +/- 1/4\\"")
            (vl-princ-to-string str)`, result:
         new Str('The \\"allowable\\" tolerance is +/- 1/4\\"')},
    ],

    errors: [
        {test: '(vl-princ-to-string)', result:
         new Error('vl-princ-to-string: too few arguments')},
        {test: '(vl-princ-to-string "abc" "def")', result:
         new Error('vl-princ-to-string: too many arguments')},
    ]
})

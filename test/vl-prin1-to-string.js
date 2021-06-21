const {TestRunner} = require('./test-runner.js')
const {Str} = require('../src/VeLispTypes.js')

// TODO: Implement +/- 1/4 ~= \261 \274

TestRunner.run({
    name: 'vl-prin1-to-string',

    // result is what .inspect shows
    tests: [
        {test: '(vl-prin1-to-string "abc")', result: new Str('"abc"')},
        {test: '(vl-prin1-to-string "/myutilities")', result: new Str('"/myutilities"')},
        {test: '(vl-prin1-to-string \'my-var)', result: new Str('MY-VAR')},
        {test: '(vl-prin1-to-string "\\r")', result: new Str('"\\r"')},
        {test: '(vl-prin1-to-string "\\n")', result: new Str('"\\n"')},
        {test: '(vl-prin1-to-string "\\t")', result: new Str('"\\t"')},
        {test: '(vl-prin1-to-string "\\e")', result: new Str('"\\e"')},
        {test: `(setq str "The \\"allowable\\" tolerance is +/- 1/4\\"")
            (vl-prin1-to-string str)`, result:
         new Str('"The \\"allowable\\" tolerance is +/- 1/4\\""')},
    ],

    errors: [
        {test: '(vl-prin1-to-string)', result:
         new Error('vl-prin1-to-string: too few arguments')},
        {test: '(vl-prin1-to-string "abc" "def")', result:
         new Error('vl-prin1-to-string: too many arguments')},
    ]
})

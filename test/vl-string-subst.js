const {TestRunner} = require('./test-runner.js')
const {Str} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-string-subst',

    tests: [
        {test: '(vl-string-subst "Obi-wan" "Ben" "Ben Kenobi")', result:
         new Str('Obi-wan Kenobi')},
        {test: '(vl-string-subst "Obi-wan" "Ben" "ben Kenobi")', result:
         new Str('ben Kenobi')},
        {test: '(vl-string-subst "Obi-wan" "Ben" "Ben Kenobi Ben")', result:
         new Str('Obi-wan Kenobi Ben')},

        {test: '(vl-string-subst "Obi-wan" "Ben" "Ben \\"Ben\\" Kenobi" 3)', result:
         new Str('Ben "Obi-wan" Kenobi')},
    ],

    errors: [
        {test: '(vl-string-subst)', result:
         new Error('vl-string-subst: too few arguments')},
        {test: '(vl-string-subst "z")', result:
         new Error('vl-string-subst: too few arguments')},
        {test: '(vl-string-subst "z" "r")', result:
         new Error('vl-string-subst: too few arguments')},

        {test: '(vl-string-subst "z" "r" "bar" 0 nil)', result:
         new Error('vl-string-subst: too many arguments')},

        {test: '(vl-string-subst 122 "r" "bar")', result:
         new Error('vl-string-subst: `new-str` expected Str')},
        
        {test: '(vl-string-subst "z" 114 "bar")', result:
         new Error('vl-string-subst: `pattern` expected Str')},

        {test: '(vl-string-subst "z" "r" \'bar)', result:
         new Error('vl-string-subst: `str` expected Str')},

        {test: '(vl-string-subst "z" "r" "bar" "1")', result:
         new Error('vl-string-subst: `start-pos` expected Int')},
        {test: '(vl-string-subst "z" "r" "bar" -1)', result:
         new Error('vl-string-subst: `start-pos` expected non-negative Int')},
    ]
})

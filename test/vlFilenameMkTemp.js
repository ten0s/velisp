const {TestRunner} = require('./test-runner.js')
//const {Str} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-filename-mktemp',

    setup: () => {},
    teardown: () => {},

    tests: [
        //{test: '(vl-filename-mktemp)', result: new Str('')},
        //{test: '(vl-filename-mktemp nil nil nil)', result: new Str('')},
    ],

    errors: [
        {test: '(vl-filename-mktemp nil nil nil nil)', result:
         new Error('vl-filename-mktemp: too many arguments')},
    ]
})

import {TestRunner} from './test-runner.js'
import {Bool, Int, Str, List} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'print',

    tests: [
        {test: '(print)', result: new Str('')},
        {test: '(print nil)', result: new Bool(false)},
        {test: '(print 1)', result: new Int(1)},
        {test: '(print "msg")', result: new Str('msg')},
        {test: '(print \'(1))', result: new List([new Int(1)])},
        // See print.{exp,lsp}
    ],

    errors: [
        {test: '(print "msg" "file" "msg")', result: new Error('print: too many arguments')},
        {test: '(print "msg" "file")', result: new Error('print: `file-desc` expected File')},
    ]
})

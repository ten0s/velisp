import {TestRunner} from './test-runner.js'
import {Bool, Int, Str, List} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'princ',

    tests: [
        {test: '(princ)', result: new Str('')},
        {test: '(princ nil)', result: new Bool(false)},
        {test: '(princ 1)', result: new Int(1)},
        {test: '(princ "msg")', result: new Str('msg')},
        {test: '(princ \'(1))', result: new List([new Int(1)])},
        // See princ.{exp,lsp}
    ],

    errors: [
        {test: '(princ "msg" "file" "msg")', result: new Error('princ: too many arguments')},
        {test: '(princ "msg" "file")', result: new Error('princ: `file-desc` expected File')},
    ]
})

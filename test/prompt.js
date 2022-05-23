import {TestRunner} from './test-runner.js'
import {Bool} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'prompt',

    tests: [
        {test: '(prompt "msg")', result: new Bool(false)},
        // See prompt.{exp,lsp}
    ],

    errors: [
        {test: '(prompt)', result: new Error('prompt: too few arguments')},
        {test: '(prompt "msg" "msg")', result: new Error('prompt: too many arguments')},
        {test: '(prompt \'msg)', result: new Error('prompt: expected Str')},
    ]
})

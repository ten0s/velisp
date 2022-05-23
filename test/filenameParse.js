import {TestRunner} from './test-runner.js'
import {Sym, Str, List, Pair} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'filename-parse',

    tests: [
        // Absolute path
        {test: '(filename-parse "/dir/name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('/')),
            new Pair(new Sym('dir'),  new Str('/dir')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(filename-parse "c:\\\\dir\\\\name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('c:\\')),    // already unescaped
            new Pair(new Sym('dir'),  new Str('c:\\dir')), // already unescaped
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(filename-parse "c:/dir/name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('c:/')),
            new Pair(new Sym('dir'),  new Str('c:/dir')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        // Relative path
        {test: '(filename-parse "dir/name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('dir')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(filename-parse "name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(filename-parse "name")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('')),
            new Pair(new Sym('base'), new Str('name')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str(''))
        ])},
        // Empty path
        {test: '(filename-parse "")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('')),
            new Pair(new Sym('base'), new Str('')),
            new Pair(new Sym('name'), new Str('')),
            new Pair(new Sym('ext'),  new Str(''))
        ])},
    ],

    errors: [
        {test: '(filename-parse)', result: new Error('filename-parse: too few arguments')},
        {test: '(filename-parse "f1" "f2")', result: new Error('filename-parse: too many arguments')},
        {test: '(filename-parse \'f1)', result: new Error('filename-parse: expected Str')},
    ]
})

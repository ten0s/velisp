const {TestRunner} = require('./test-runner.js')
const {Sym, Str, List, Pair} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 've-filename-parse',

    tests: [
        // Absolute path
        {test: '(ve-filename-parse "/dir/name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('/')),
            new Pair(new Sym('dir'),  new Str('/dir')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(ve-filename-parse "c:\\\\dir\\\\name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('c:\\\\')),
            new Pair(new Sym('dir'),  new Str('c:\\\\dir')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(ve-filename-parse "c:/dir/name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('c:/')),
            new Pair(new Sym('dir'),  new Str('c:/dir')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        // Relative path
        {test: '(ve-filename-parse "dir/name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('dir')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(ve-filename-parse "name.ext")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('')),
            new Pair(new Sym('base'), new Str('name.ext')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str('.ext'))
        ])},
        {test: '(ve-filename-parse "name")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('')),
            new Pair(new Sym('base'), new Str('name')),
            new Pair(new Sym('name'), new Str('name')),
            new Pair(new Sym('ext'),  new Str(''))
        ])},
        // Empty path
        {test: '(ve-filename-parse "")', result: new List([
            new Pair(new Sym('root'), new Str('')),
            new Pair(new Sym('dir'),  new Str('')),
            new Pair(new Sym('base'), new Str('')),
            new Pair(new Sym('name'), new Str('')),
            new Pair(new Sym('ext'),  new Str(''))
        ])},
    ],

    errors: [
        {test: '(ve-filename-parse)', result: new Error('ve-filename-parse: too few arguments')},
        {test: '(ve-filename-parse "f1" "f2")', result: new Error('ve-filename-parse: too many arguments')},
        {test: '(ve-filename-parse \'f1)', result: new Error('ve-filename-parse: expected Str')},
    ]
})

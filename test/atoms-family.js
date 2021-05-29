const {TestRunner} = require('./test-runner.js')
const {Bool, Str, Sym, List} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'atoms-family',

    tests: [
        {test: '(atoms-family 0 \'())', result: new List([])},
        {test: '(atoms-family 0 ())', result: new List([])},
        {test: '(atoms-family 0 nil)', result: new List([])},

        {test: '(atoms-family 0)', result: (act) => {
            return act instanceof List
        }},
        {test: '(atoms-family 1)', result: (act) => {
            return act instanceof List
        }},

        {test: '(equal (atoms-family 1) (atoms-family 99))', result: new Bool(true)},

        {test: '(atoms-family 0 \'("CAR" "CDR" "XYZ"))', result: new List([
            new Sym('car'), new Sym('cdr'), new Bool(false)
        ])},
        {test: '(atoms-family 0 \'("car" "cdr" "xyz"))', result: new List([
            new Sym('car'), new Sym('cdr'), new Bool(false)
        ])},
        {test: '(atoms-family 0 \'("cdr" "car" "cdr" "xyz"))', result: new List([
            new Sym('cdr'), new Sym('car'), new Sym('cdr'), new Bool(false)
        ])},

        {test: '(defun xyz () (atoms-family 1 \'("CAR" "CDR" "XYZ"))) (xyz)', result: new List([
            new Str('CAR'), new Str('CDR'), new Str('XYZ')
        ])},
    ],

    errors: [
        {test: '(atoms-family)', result: new Error('atoms-family: too few arguments')},
        {test: '(atoms-family "0")', result: new Error('atoms-family: `format` expected Int')},
        {test: '(atoms-family -1)', result: new Error('atoms-family: `format` expected non-negative Int')},
        {test: '(atoms-family 0 "")', result: new Error('atoms-family: `symlist` expected List')},
        {test: '(atoms-family 0 \'(0))', result: new Error('atoms-family: `symlist` expected List of Str')},
        {test: '(atoms-family 0 nil nil)', result: new Error('atoms-family: too many arguments')},
    ]
})

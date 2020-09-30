const QUnit = require('qunit');
const {evaluate} = require('../src/VeLispEvaluator.js');
const {Str} = require('../src/VeLispTypes.js');

// TODO: Implement +/- 1/4 ~= \261 \274

const tests = [
    {test: '(vl-prin1-to-string "abc")', result: new Str('\\"abc\\"')},
    {test: '(vl-prin1-to-string "/myutilities")', result: new Str('\\"/myutilities\\"')},
    {test: '(vl-prin1-to-string \'my-var)', result: new Str('MY-VAR')},
    {test: `(setq str "The \\"allowable\\" tolerance is +/- 1/4\\"")
            (vl-prin1-to-string str)`,
     result: new Str('\\"The \\"allowable\\" tolerance is +/- 1/4\\"\\"')},
    
    {test: '(vl-princ-to-string "abc")', result: new Str('abc')},
    {test: '(vl-princ-to-string "/myutilities")', result: new Str('/myutilities')},
    {test: '(vl-princ-to-string \'my-var)', result: new Str('MY-VAR')},
    {test: `(setq str "The \\"allowable\\" tolerance is +/- 1/4\\"")
            (vl-princ-to-string str)`,
     result: new Str('The "allowable" tolerance is +/- 1/4"')},
];

const errors = [
    {test: '(vl-prin1-to-string)',
     result: new Error('vl-prin1-to-string: too few arguments')},
    {test: '(vl-prin1-to-string "abc" "def")',
     result: new Error('vl-prin1-to-string: too many arguments')},
    
    {test: '(vl-princ-to-string)',
     result: new Error('vl-princ-to-string: too few arguments')},
    {test: '(vl-princ-to-string "abc" "def")',
     result: new Error('vl-princ-to-string: too many arguments')},
];
    
QUnit.test('vl-prin{1,c}-to-string', assert => {
    tests.forEach(t => {
        assert.deepEqual(evaluate(t.test), t.result, t.test)
    });

    errors.forEach(t => {
        assert.throws(() => evaluate(t.test), t.result, t.test)
    });
});

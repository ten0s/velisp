const {Bool, Str, Sym, Fun, ensureType} = require('../VeLispTypes.js');
const Evaluator = require('../VeLispEvaluator.js');

//
// I/O Functions
//

exports.initContext = function (context) {
    context.setSym('PROMPT', new Fun('prompt', ['msg'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('prompt: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('prompt: too many arguments');
        }
        const arg = ensureType('prompt:', args[0], [Str]);
        console.log(arg.toEscapedString());
        return new Bool(false);
    }));
    context.setSym('PRIN1', new Fun('prin1', ['[expr [file-desc]]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('prin1: too many arguments');
        }
        if (args.length == 0) {
            // TODO: should return some null symbol
            return new Str('');
        }
        const arg = args[0];
        // TODO: file-desc
        console.log(arg.toString());
        return arg;
    }));
    context.setSym('PRINC', new Fun('princ', ['[expr [file-desc]]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('princ: too many arguments');
        }
        if (args.length == 0) {
            // TODO: should return some null symbol
            return new Str('');
        }
        const arg = args[0];
        // TODO: file-desc
        if (arg instanceof Str) {
            console.log(arg.toEscapedString());
        } else {
            console.log(arg.toString());
        }
        return arg;
    }));
    context.setSym('PRINT', new Fun('print', ['[expr [file-desc]]'], [], (self, args) => {
        if (args.length > 2) {
            throw new Error('print: too many arguments');
        }
        if (args.length == 0) {
            // TODO: should return some null symbol
            return new Str('');
        }
        const arg = args[0];
        // TODO: file-desc
        console.log('\n' + arg.toString() + ' ');
        return arg;
    }));
    context.setSym('GETENV', new Fun('getenv', ['name'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('getenv: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('getenv: too many arguments');
        }
        const name = ensureType('getenv:', args[0], [Str]);
        const value = process.env[name.value()];
        if (typeof value === 'undefined') {
            return new Bool(false);
        }
        return new Str(value);
    }));
    context.setSym('SETENV', new Fun('setenv', ['name', 'value'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('setenv: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('setenv: too many arguments');
        }
        const name = ensureType('setenv: `name`', args[0], [Str]);
        const value = ensureType('setenv: `value`', args[1], [Str]);
        process.env[name.value()] = value.value();
        return value;
    }));
}

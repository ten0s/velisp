const {Int, Real, Str, Fun, ensureType} = require('../VeLispTypes.js');

//
// String-Handling Functions
//

exports.initContext = function (context) {
    context.setSym('ITOA', new Fun('itoa', ['int'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('itoa: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('itoa: too many arguments');
        }
        return new Str(ensureType('itoa', args[0], [Int]).toString());
    }));
    context.setSym('ATOI', new Fun('atoi', ['str'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('atoi: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('atoi: too many arguments');
        }
        const arg = ensureType('atoi', args[0], [Str]);
        try {
            const val = Number.parseInt(arg.str);
            if (Number.isFinite(val)) {
                return new Int(val);
            }
        } catch (e) {}
        throw new Error('atoi: conversion impossible');
    }));
    context.setSym('ATOF', new Fun('atof', ['str'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('atof: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('atof: too many arguments');
        }
        const arg = ensureType('atof', args[0], [Str]);
        try {
            const val = parseFloat(arg.str);
            if (Number.isFinite(val)) {
                return new Real(val);
            }
        } catch (e) {}
        throw new Error('atof: conversion impossible');
    }));
}

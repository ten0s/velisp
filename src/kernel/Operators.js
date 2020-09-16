const {Bool, Int, List, Pair, Fun} = require('../VeLispTypes.js');

//
// Operators (AutoCAD 2013 AutoLISP Reference Guild p.1)
//

exports.initContext = function (context) {
    context.setSym('*', new Fun('*', ['[num] ...'], [], (self, args) => {
        if (args.length == 0) {
            return new Int(0);
        }
        let result = new Int(1);
        for (let i = 0; i < args.length; i++) {
            result = result.multiply(args[i]);
        }
        return result;
    }));
    context.setSym('/', new Fun('/', ['[num] ...'], [], (self, args) => {
        if (args.length == 0) {
            return new Int(0);
        }
        let result = args[0];
        for (let i = 1; i < args.length; i++) {
            result = result.divide(args[i]);
        }
        return result;
    }));
    context.setSym('+', new Fun('+', ['[num] ...'], [], (self, args) => {
        let result = new Int(0);
        for (let i = 0; i < args.length; i++) {
            result = result.add(args[i]);
        }
        return result;
    }));
    context.setSym('-', new Fun('-', ['[num] ...'], [], (self, args) => {
        if (args.length == 0) {
            return new Int(0);
        }
        let result = args[0];
        if (args.length == 1) {
            return result.multiply(new Int(-1));
        }
        for (let i = 1; i < args.length; i++) {
            result = result.subtract(args[i]);
        }
        return result;
    }));
    context.setSym('=', new Fun('=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('=: too few arguments');
        }
        let result = new Bool(true);
        let val1 = args[0];
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i];
            result = val1.equalTo(val2);
            if (result.isNil()) break;
            val1 = val2;
        }
        return result;
    }));
    context.setSym('/=', new Fun('/=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('/=: too few arguments');
        }
        let result = new Bool(true);
        let val1 = args[0];
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i];
            result = val1.equalTo(val2).not();
            if (result.isNil()) break;
            val1 = val2;
        }
        return result;
    }));
    context.setSym('<', new Fun('<', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('<: too few arguments');
        }
        let result = new Bool(true);
        let val1 = args[0];
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i];
            result = val1.lessThan(val2);
            if (result.isNil()) break;
            val1 = val2;
        }
        return result;
    }));
    context.setSym('<=', new Fun('<=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('<=: too few arguments');
        }
        let result = new Bool(true);
        let val1 = args[0];
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i];
            result = val1.lessThan(val2).or(val1.equalTo(val2));
            if (result.isNil()) break;
            val1 = val2;
        }
        return result;
    }));
    context.setSym('>', new Fun('>', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('>: too few arguments');
        }
        let result = new Bool(true);
        let val1 = args[0];
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i];
            result = val1.lessThan(val2).or(val1.equalTo(val2)).not();
            if (result.isNil()) break;
            val1 = val2;
        }
        return result;
    }));
    context.setSym('>=', new Fun('>=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('>=: too few arguments');
        }
        let result = new Bool(true);
        let val1 = args[0];
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i];
            result = val1.lessThan(val2).not();
            if (result.isNil()) break;
            val1 = val2;
        }
        return result;
    }));
    context.setSym('~', new Fun('~', ['int'], [], (self, args) => {
        if (args.length == 0) {
            throw new Error('~: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('~: too many arguments');
        }
        if (args[0] instanceof Int) {
            return args[0].bitwiseNot();
        }
        throw new Error('~: expected Int');
    }));
    // TODO: re-impl in lisp
    context.setSym('1+', new Fun('1+', ['num'], [], (self, args) => {
        return args[0].add(new Int(1));
    }));
    // TODO: re-impl in lisp
    context.setSym('1-', new Fun('1-', ['num'], [], (self, args) => {
        return args[0].subtract(new Int(1));
    }));
}

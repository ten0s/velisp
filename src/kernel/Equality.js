const {Bool, Fun} = require('../VeLispTypes.js');

//
// Equality Functions
//

exports.initContext = function (context) {
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
    context.setSym('EQ', new Fun('eq', ['expr1 expr2'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('eq: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('eq: too many arguments');
        }
        // Referencial Equality, but treat Bool as a special case
        if (typeof args[0] != typeof args[1]) {
            return new Bool(false);
        }
        if (args[0] instanceof Bool) {
            return new Bool(args[0].bool === args[1].bool);
        }
        return new Bool(args[0] === args[1]);
    }));
}

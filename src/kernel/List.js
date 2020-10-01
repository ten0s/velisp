const {Bool, List, Pair, Fun, ensureType} = require('../VeLispTypes.js');

exports.initContext = function (context) {
    context.setSym('APPEND', new Fun('append', ['[list ...]'], [], (self, args) => {
        if (args.length === 0) {
            return new Bool(false);
        }
        let result = new List([]);
        for (let i = 0; i < args.length; i++) {
            if (args[i].isNil()) {
                continue;
            }
            const list = ensureType('append:', args[i], [List]);
            result = result.concat(list);
        }
        return result;
    }));
    context.setSym('CAR', new Fun('car', ['listorpair'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('car: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('car: too many arguments');
        }
        const listOrPair = args[0];
        if (listOrPair instanceof List && listOrPair.length() > 0) {
            return listOrPair.car();
        }
        if (listOrPair instanceof Pair) {
            return listOrPair.car();
        }
        throw new Error('car: expected non-empty List, Pair');
    }));
    context.setSym('CDR', new Fun('cdr', ['listorpair'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('cdr: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('cdr: too many arguments');
        }
        const listOrPair = args[0];
        if (listOrPair instanceof List && listOrPair.length() > 0) {
            return listOrPair.cdr();
        }
        if (listOrPair instanceof Pair) {
            return listOrPair.cdr();
        }
        throw new Error('cdr: expected non-empty List, Pair');
    }));
    context.setSym('CONS', new Fun('cons', ['first', 'listoratom'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('cons: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('cons: too many arguments');
        }
        const fst = args[0];
        const snd = args[1];
        if (snd instanceof List || snd instanceof Pair) {
            return snd.cons(fst);
        } else if (snd.isNil()) {
            return new List([fst]);
        } else {
            return new Pair(fst, snd);
        }
    }));
    context.setSym('LIST', new Fun('list', ['[expr ...]'], [], (self, args) => {
        const result = [];
        for (let i = 0; i < args.length; i++) {
            result.push(args[i]);
        }
        return new List(result);
    }));
}

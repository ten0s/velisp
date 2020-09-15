const {Bool, List, Pair, Fun} = require('../VeLispTypes.js');

//
// List Functions
//

exports.initContext = function (context) {
    context.setSym('list', new Fun('list', ['[expr ...]'], function (self, args) {
        const result = [];
        for (let i = 0; i < args.length; i++) {
            result.push(args[i]);
        }
        return new List(result);
    }));
    context.setSym('listp', new Fun('listp', ['item'], function (self, args) {
        if (args.length == 0) {
            throw new Error('listp: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('listp: too many arguments');
        }
        const item = args[0];
        if (item instanceof List || item.isNil()) {
            return new Bool(true);
        }
        return new Bool(false);
    }));
    context.setSym('cons', new Fun('cons', ['first', 'listoratom'], function (self, args) {
        if (args.length < 2) {
            throw new Error('cons: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('cons: too many arguments');
        }
        const car = args[0];
        const cdr = args[1];
        if (cdr instanceof List) {
            return cdr.cons(car);
        } else if (cdr.isNil()) {
            return new List([car]);
        } else {
            return new Pair(car, cdr);
        }
    }));
    context.setSym('car', new Fun('car', ['listorpair'], function (self, args) {
        if (args.length == 0) {
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
        throw new Error('car: must be non-empty List or Pair');
    }));
    context.setSym('cdr', new Fun('cdr', ['listorpair'], function (self, args) {
        if (args.length == 0) {
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
        throw new Error('cdr: must be non-empty List or Pair');
    }));
}

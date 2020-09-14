const {Bool, List, Pair, Fun} = require('../VeLispTypes.js');

//
// List Functions
//

exports.addTo = function (context) {
    context.setSym('list', new Fun('list', ['[expr ...]'], function (self, args) {
        const result = [];
        for (let i = 0; i < args.length; i++) {
            result.push(args[i]);
        }
        return new List(result);
    }));
    context.setSym('listp', new Fun('listp', ['item'], function (self, args) {
        const item = args[0];
        if (item instanceof List || item.isNil()) {
            return new Bool(true);
        }
        return new Bool(false);
    }));
    context.setSym('cons', new Fun('cons', ['first', 'listoratom'], function (self, args) {
        const fst = args[0];
        const snd = args[1];
        if (snd instanceof List) {
            return snd.cons(fst);
        } else if (snd.isNil()) {
            return new List([fst]);
        } else {
            return new Pair(fst, snd);
        }
    }));
    context.setSym('car', new Fun('car', ['listorpair'], function (self, args) {
        const listOrPair = args[0];
        return listOrPair.car();
    }));
    context.setSym('cdr', new Fun('cdr', ['listorpair'], function (self, args) {
        const listOrPair = args[0];
        return listOrPair.cdr();
    }));
}

const {Bool, Fun} = require('../VeLispTypes.js');

//
// Symbol-Handling Functions
//

exports.initContext = function (context) {
    context.setSym('NOT', new Fun('not', ['item'], function (self, args) {
        if (args.length == 0) {
            throw new Error('not: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('not: too many arguments');
        }
        return new Bool(args[0].isNil());
    }));
    context.setSym('NULL', new Fun('null', ['item'], function (self, args) {
        if (args.length == 0) {
            throw new Error('null: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('null: too many arguments');
        }
        return new Bool(args[0].isNil());
    }));
}

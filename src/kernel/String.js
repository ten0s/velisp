const {Int, Str, Fun, ensureType} = require('../VeLispTypes.js');

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
}

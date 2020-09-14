const {Bool, Fun} = require('../VeLispTypes.js');

//
// Symbol-Handling Functions
//

exports.addTo = function (context) {
    context.setSym('not', new Fun('not', ['item'], function (self, args) {
        return new Bool(args[0].isNil());
    }));
    context.setSym('null', context.getSym('not'));
}

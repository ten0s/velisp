const {Bool, Real} = require('../VeLispTypes.js');

exports.initContext = function (context) {
    context.setSym('T', new Bool(true));
    context.setSym('PI', new Real(Math.PI));
}

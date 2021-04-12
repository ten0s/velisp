const {EOL} = require('os')
const {Bool, Real, Str} = require('../VeLispTypes.js')

exports.initContext = (context) => {
    context.setSym('T', new Bool(true))
    context.setSym('PI', new Real(Math.PI))
    // VeLisp Extension
    context.setSym('EOL', new Str(EOL.split('\r').join('\\r').split('\n').join('\\n')))
}

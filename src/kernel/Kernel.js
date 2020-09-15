const Application = require('./Application.js');
const Function = require('./Function.js');
const List = require('./List.js');
const Operators = require('./Operators.js');
const Symbol = require('./Symbol.js');

exports.initContext = function (context) {
    [
        Application,
        Function,
        List,
        Operators,
        Symbol
    ].forEach(module => module.initContext(context));
}
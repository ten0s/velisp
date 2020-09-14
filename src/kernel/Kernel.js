const Operators = require('./Operators.js');
const List = require('./List.js');
const Symbol = require('./Symbol.js');

exports.addTo = function (context) {
    [Operators, List, Symbol].forEach(module => module.addTo(context));
}

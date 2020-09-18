exports.initContext = function (context) {
    [
        require('./Application.js'),
        require('./Arithmetic.js'),
        require('./Equality.js'),
        require('./Function.js'),
        require('./List.js'),
        require('./Symbol.js')
    ].forEach(
        module => module.initContext(context)
    );
}

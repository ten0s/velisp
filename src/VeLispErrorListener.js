const antlr4 = require('antlr4')

class VeLispErrorListener extends antlr4.error.ErrorListener {
    syntaxError(recognizer, symbol, line, column, message, _payload) {
        throw new Error(`line: ${line}, column: ${column}, message: ${message}`)
    }
}

module.exports = VeLispErrorListener

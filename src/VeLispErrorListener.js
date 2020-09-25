const antlr4 = require('antlr4');

class VeLispErrorListener extends antlr4.error.ErrorListener {
  syntaxError(recognizer, symbol, line, column, message, payload) {
    throw new Error(`line: ${line}, column: ${column}, message: ${message}`);
  }
}

exports.VeLispErrorListener = VeLispErrorListener;

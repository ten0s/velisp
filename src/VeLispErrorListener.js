import antlr4 from 'antlr4'
import {makeError} from './VeLispError.js'

class VeLispErrorListener extends antlr4.error.ErrorListener {
    constructor(context) {
        super()
        this.context = context
    }
    syntaxError(recognizer, symbol, line, column, message, _payload) {
        throw new Error(makeError(
            `line: ${line} column: ${column} message: ${message}`,
            this.context
        ))
    }
}

export default VeLispErrorListener

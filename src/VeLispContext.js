const {Bool} = require('./VeLispTypes.js')

// VeLisp is a Lisp-1
class VeLispContext {
    constructor(parent = null) {
        this.parent = parent
        this.symbols = {}
    }

    // Init variable in the current context only.
    initVar(name, value) {
        //console.log(`initVar(${name}, ${value})`, this);
        this.symbols[name] = value
    }

    // Set variable into the current context, if defined.
    // If not defined, set into the parent context, if available.
    // Otherwise, set into the current (global) context.
    setVar(name, value) {
        //console.log(`setVar(${name}, ${value})`, this);
        if (this.symbols[name]) {
            this.symbols[name] = value
        } else if (this.parent) {
            this.parent.setVar(name, value)
        } else {
            this.symbols[name] = value
        }
    }

    // Get variable from the current context, if defined.
    // If not defined, get variable from the parent context, if available.
    // Otherwise, return nil.
    getVar(name) {
        let value = this.symbols[name]
        if (value) {
            return value
        }
        if (this.parent) {
            return this.parent.getVar(name)
        }
        return new Bool(false)
    }

    // Set symbol into the current context, if defined.
    // If not defined, set into the parent context, if available.
    // Otherwise, set into the current (global) context.
    setSym(name, value) {
        this.setVar(name, value)
    }

    // Get symbol from the current context, if defined.
    // If not defined, get symbol from the parent context, if available.
    // Otherwise, return nil.
    getSym(name) {
        return this.getVar(name)
    }
}

module.exports = VeLispContext

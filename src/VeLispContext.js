const {Bool} = require('./VeLispTypes.js');

class GlobalContext {
    constructor() {
        // Symbols
        this.syms = {};
        // Variables
        this.vars = {};
    }

    setVar(name, value) {
        //console.log(`setVar(${name}, ${value})`, this);
        this.vars[name] = value;
    }

    getVar(name) {
        let value = this.vars[name];
        if (value) {
            return value;
        }
        return new Bool(false);
    }

    setSym(name, value) {
        this.syms[name] = value;
    }

    getSym(name) {
        let value = this.syms[name];
        if (value) {
            return value;
        }
        return new Bool(false);
    }
}

class Context {
    constructor(parent) {
        this.parent = parent;
        this.syms = {};
        this.vars = {};
    }

    // Init variable in the current context only.
    initVar(name, value) {
        //console.log(`initVar(${name}, ${value})`, this);
        this.vars[name] = value;
    }

    // Set variable into the current context, if defined.
    // If not defined, set into the parent context.
    setVar(name, value) {
        //console.log(`setVar(${name}, ${value})`, this);
        if (this.vars[name]) {
            this.vars[name] = value;
        } else {
            this.parent.setVar(name, value);
        }
    }

    // Get variable from the current context, if defined.
    // If not defined, get variable from the parent context.
    getVar(name) {
        let value = this.vars[name];
        if (value) {
            return value;
        }
        return this.parent.getVar(name);
    }

    // Set/Init variable in the current context only.
    setSym(name, value) {
        this.syms[name] = value;
    }

    // Get symbol from the current context, if defined.
    // If not defined, get symbol from the parent context.
    getSym(name) {
        let value = this.syms[name];
        if (value) {
            return value;
        }
        return this.parent.getSym(name);
    }
}

exports.GlobalContext = GlobalContext;
exports.Context = Context;

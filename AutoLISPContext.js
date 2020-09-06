import {Bool, Int, Fun} from './AutoLISPTypes.js';

export class AutoLISPGlobalContext {
    constructor() {
        this.syms = {};
        this.syms["1+"] = new Fun('1+', ['num'], function (self, args) {
            return args[0].add(new Int(1));
        });
        this.syms["1-"] = new Fun('1-', ['num'], function (self, args) {
            return args[0].subtract(new Int(1));
        });
        this.vars = {};
    }

    setVar(name, value) {
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

export class AutoLISPContext {
    constructor(parent) {
        this.parent = parent;
        this.syms = {};
        this.vars = {};
    }

    setVar(name, value) {
        this.vars[name] = value;
    }

    getVar(name) {
        let value = this.vars[name];
        if (value) {
            return value;
        }
        if (this.parent) {
            return this.parent.getVar(name);
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
        if (this.parent) {
            return this.parent.getSym(name);
        }
        return new Bool(false);
    }
}

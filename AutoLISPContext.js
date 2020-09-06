import {Bool, Int, List, Fun} from './AutoLISPTypes.js';

export class AutoLISPGlobalContext {
    constructor() {
        // Symbols
        this.syms = {};
        // Variables
        this.vars = {};

        //
        // Kernel Operators (AutoCAD 2013 AutoLISP Reference Guild p.1)
        //
        this.syms['*'] = new Fun('*', ['[num] ...'], function (self, args) {
            if (args.length == 0) {
                return new Int(0);
            }
            let result = new Int(1);
            for (let i = 0; i < args.length; i++) {
                result = result.multiply(args[i]);
            }
            return result;
        });
        this.syms['/'] = new Fun('/', ['[num] ...'], function (self, args) {
            if (args.length == 0) {
                return new Int(0);
            }
            let result = args[0];
            for (let i = 1; i < args.length; i++) {
                result = result.divide(args[i]);
            }
            return result;
        });
        this.syms['+'] = new Fun('+', ['[num] ...'], function (self, args) {
            let result = new Int(0);
            for (let i = 0; i < args.length; i++) {
                result = result.add(args[i]);
            }
            return result;
        });
        this.syms['-'] = new Fun('-', ['[num] ...'], function (self, args) {
            if (args.length == 0) {
                return new Int(0);
            }
            let result = args[0];
            if (args.length == 1) {
                return result.multiply(new Int(-1));
            }
            for (let i = 1; i < args.length; i++) {
                result = result.subtract(args[i]);
            }
            return result;
        });
        this.syms['='] = new Fun('=', ['numstr [numstr] ...'], function (self, args) {
            let result = new Bool(true);
            let val1 = args[0];
            for (let i = 1; i < args.length; i++) {
                const val2 = args[i];
                result = val1.equalTo(val2);
                if (result.isNil()) break;
                val1 = val2;
            }
            return result;
        });
        this.syms['/='] = new Fun('/=', ['numstr [numstr] ...'], function (self, args) {
            let result = new Bool(true);
            let val1 = args[0];
            for (let i = 1; i < args.length; i++) {
                const val2 = args[i];
                result = val1.equalTo(val2).not();
                if (result.isNil()) break;
                val1 = val2;
            }
            return result;
        });
        this.syms['<'] = new Fun('<', ['numstr [numstr] ...'], function (self, args) {
            let result = new Bool(true);
            let val1 = args[0];
            for (let i = 1; i < args.length; i++) {
                const val2 = args[i];
                result = val1.lessThan(val2);
                if (result.isNil()) break;
                val1 = val2;
            }
            return result;
        });
        this.syms['<='] = new Fun('<=', ['numstr [numstr] ...'], function (self, args) {
            let result = new Bool(true);
            let val1 = args[0];
            for (let i = 1; i < args.length; i++) {
                const val2 = args[i];
                result = val1.lessThan(val2).or(val1.equalTo(val2));
                if (result.isNil()) break;
                val1 = val2;
            }
            return result;
        });
        this.syms['>'] = new Fun('>', ['numstr [numstr] ...'], function (self, args) {
            let result = new Bool(true);
            let val1 = args[0];
            for (let i = 1; i < args.length; i++) {
                const val2 = args[i];
                result = val1.lessThan(val2).or(val1.equalTo(val2)).not();
                if (result.isNil()) break;
                val1 = val2;
            }
            return result;
        });
        this.syms['>='] = new Fun('>=', ['numstr [numstr] ...'], function (self, args) {
            let result = new Bool(true);
            let val1 = args[0];
            for (let i = 1; i < args.length; i++) {
                const val2 = args[i];
                result = val1.lessThan(val2).not();
                if (result.isNil()) break;
                val1 = val2;
            }
            return result;
        });
        this.syms['~'] = new Fun('~', ['int'], function (self, args) {
            return args[0].bitwiseNot();
        });
        // TODO: re-impl in lisp
        this.syms['1+'] = new Fun('1+', ['num'], function (self, args) {
            return args[0].add(new Int(1));
        });
        // TODO: re-impl in lisp
        this.syms['1-'] = new Fun('1-', ['num'], function (self, args) {
            return args[0].subtract(new Int(1));
        });
        //
        // Kernel List Functions
        //
        this.syms['list'] = new Fun('list', ['[expr ...]'], function (self, args) {
            let result = [];
            for (let i = 0; i < args.length; i++) {
                result.push(args[i]);
            }
            return new List(result);
        });
        this.syms['car'] = new Fun('car', ['list'], function (self, args) {
            let list = args[0];
            return list.car();
        });
        this.syms['cdr'] = new Fun('cdr', ['list'], function (self, args) {
            let list = args[0];
            return list.cdr();
        });
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

export class AutoLISPContext {
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

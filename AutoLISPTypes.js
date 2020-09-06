export class Bool {
    constructor(val) {
        this.val = val;
    }

    // :: () -> true | false
    isTruthy() {
        return this.val;
    }

    // :: () -> true | false
    isFalsy() {
        return !this.val;
    }

    and(that) {
        if (that instanceof Bool) {
            return new Bool(this.val && that.val);
        }
        throw new Error(`Not implemented ${this} and ${that}`);
    }

    or(that) {
        if (that instanceof Bool) {
            return new Bool(this.val || that.val);
        }
        throw new Error(`Not implemented ${this} or ${that}`);
    }

    not() {
        return new Bool(!this.val);
    }

    toString() {
        return this.val ? "T" : "nil";
    }
}

export class Int {
    constructor(val) {
        this.val = val;
    }

    // :: () -> true
    isTruthy() {
        return true;
    }

    // :: () -> false
    isFalsy() {
        return false;
    }

    value() {
        return this.val;
    }

    multiply(that) {
        if (that instanceof Int) {
            return new Int(this.val * that.val);
        } else if (that instanceof Real) {
            return new Real(this.val * that.val);
        }
        throw new Error(`Not implemented ${this} * ${that}`);
    }

    divide(that) {
        if (that instanceof Int) {
            const res = this.val / that.val;
            if (Number.isInteger(res)) {
                return new Int(res);
            } else {
                return new Int(Math.floor(res));
            }            
        } else if (that instanceof Real) {
            return new Real(this.val / that.val);
        }
        throw new Error(`Not implemented ${this} / ${that}`);
    }

    add(that) {
        if (that instanceof Int) {
            return new Int(this.val + that.val);
        } else if (that instanceof Real) {
            return new Real(this.val + that.val);
        }
        throw new Error(`Not implemented ${this} + ${that}`);
    }

    subtract(that) {
        if (that instanceof Int) {
            return new Int(this.val - that.val);
        } else if (that instanceof Real) {
            return new Real(this.val - that.val);
        }
        throw new Error(`Not implemented ${this} - ${that}`);
    }

    equalTo(that) {
        if (that instanceof Int) {
            return new Bool(this.val === that.val);
        } else if (that instanceof Real) {
            return new Bool(this.val === that.val);
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    lessThan(that) {
        if (that instanceof Int) {
            return new Bool(this.val < that.val);
        } else if (that instanceof Real) {
            return new Bool(this.val < that.val);
        }
        throw new Error(`Not implemented ${this} < ${that}`);
    }

    bitwiseNot() {
        return new Int(~this.val);
    }

    toString() {
        return this.val.toString();
    }
}

export class Real {
    constructor(val) {
        this.val = val;
    }

    // :: () -> true
    isTruthy() {
        return true;
    }

    // :: () -> false
    isFalsy() {
        return false;
    }

    multiply(that) {
        if (that instanceof Int) {
            return new Real(this.val * that.val);
        } else if (that instanceof Real) {
            return new Real(this.val * that.val);
        }
        throw new Error(`Not implemented ${this} * ${that}`);
    }

    divide(that) {
        if (that instanceof Int) {
            return new Real(this.val / that.val);
        } else if (that instanceof Real) {
            return new Real(this.val / that.val);
        }
        throw new Error(`Not implemented ${this} / ${that}`);
    }

    add(that) {
        if (that instanceof Int) {
            return new Real(this.val + that.val);
        } else if (that instanceof Real) {
            return new Real(this.val + that.val);
        }
        throw new Error(`Not implemented ${this} + ${that}`);
    }

    subtract(that) {
        if (that instanceof Int) {
            return new Real(this.val - that.val);
        } else if (that instanceof Real) {
            return new Real(this.val - that.val);
        }
        throw new Error(`Not implemented ${this} - ${that}`);
    }

    equalTo(that) {
        if (that instanceof Int) {
            return new Bool(this.val === that.val);
        } else if (that instanceof Real) {
            return new Bool(this.val === that.val);
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    lessThan(that) {
        if (that instanceof Int) {
            return new Bool(this.val < that.val);
        } else if (that instanceof Real) {
            return new Bool(this.val < that.val);
        }
        throw new Error(`Not implemented ${this} < ${that}`);
    }

    toString() {
        if (Number.isInteger(this.val)) { 
            return this.val + '.0'
        } else {
            return this.val.toString(); 
        }
    }
}

export class Str {
    constructor(str) {
        this.val = str;
    }

    // :: () -> true
    isTruthy() {
        return true;
    }

    // :: () -> false
    isFalsy() {
        return false;
    }

    equalTo(that) {
        if (that instanceof Str) {
            return new Bool(this.val === that.val);
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    lessThan(that) {
        if (that instanceof Str) {
            return new Bool(this.val < that.val);
        }
        throw new Error(`Not implemented ${this} < ${that}`);
    }

    toString() {
        return this.val;
    }
}

export class List {
    constructor(arr) {
        // TODO: who should make copy, see cdr
        this.val = [...arr];
    }

    // :: () -> true | false
    isTruthy() {
        return this.val.length !== 0;
    }

    // :: () -> true | false
    isFalsy() {
        return this.val.length === 0;
    }

    car() {
        // TODO: check length
        return this.val[0];
    }

    cdr() {
        // TODO: check length
        const [, ...rest] = this.val;
        return new List(rest);
    }

    toString() {
        return `(${this.val.join(' ')})`;
    }
}

export class Fun {
    constructor(name, params, fun) {
        this.name = name;
        this.params = params;
        this.fun = fun;
    }

    // :: () -> true
    isTruthy() {
        return true;
    }

    // :: () -> false
    isFalsy() {
        return false;
    }

    apply(evaluator, args) {
        return this.fun(evaluator, args);
    }

    toString() {
        return `(${this.name} ${params.join(' ')})`;
    }
}

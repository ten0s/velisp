class Bool {
    constructor(bool) {
        this.bool = bool;
    }

    // :: () -> true | false
    isNil() {
        return !this.bool;
    }

    and(that) {
        if (that instanceof Bool) {
            return new Bool(this.bool && that.bool);
        }
        throw new Error(`Not implemented ${this} and ${that}`);
    }

    or(that) {
        if (that instanceof Bool) {
            return new Bool(this.bool || that.bool);
        }
        throw new Error(`Not implemented ${this} or ${that}`);
    }

    not() {
        return new Bool(!this.bool);
    }

    equalTo(that) {
        if (that instanceof Bool) {
            return new Bool(this.bool === that.bool);
        }
        if (that instanceof List) {
            if (that.isNil()) {
                return new Bool(this.isNil());
            }
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    toString() {
        return this.bool ? "T" : "nil";
    }
}

class Int {
    constructor(int) {
        this.int = int;
    }

    // :: () -> false
    isNil() {
        return false;
    }

    value() {
        return this.int;
    }

    multiply(that) {
        if (that instanceof Int) {
            return new Int(this.int * that.int);
        }
        if (that instanceof Real) {
            return new Real(this.int * that.real);
        }
        throw new Error(`Not implemented ${this} * ${that}`);
    }

    divide(that) {
        if (that instanceof Int) {
            const res = this.int / that.int;
            if (Number.isInteger(res)) {
                return new Int(res);
            }
            return new Int(Math.floor(res));
        }
        if (that instanceof Real) {
            return new Real(this.int / that.real);
        }
        throw new Error(`Not implemented ${this} / ${that}`);
    }

    add(that) {
        if (that instanceof Int) {
            return new Int(this.int + that.int);
        }
        if (that instanceof Real) {
            return new Real(this.int + that.real);
        }
        throw new Error(`Not implemented ${this} + ${that}`);
    }

    subtract(that) {
        if (that instanceof Int) {
            return new Int(this.int - that.int);
        }
        if (that instanceof Real) {
            return new Real(this.int - that.real);
        }
        throw new Error(`Not implemented ${this} - ${that}`);
    }

    equalTo(that) {
        if (that instanceof Int) {
            return new Bool(this.int === that.int);
        }
        if (that instanceof Real) {
            return new Bool(this.int === that.real);
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    lessThan(that) {
        if (that instanceof Int) {
            return new Bool(this.int < that.int);
        }
        if (that instanceof Real) {
            return new Bool(this.int < that.real);
        }
        throw new Error(`Not implemented ${this} < ${that}`);
    }

    bitwiseNot() {
        return new Int(~this.int);
    }

    toString() {
        return this.int.toString();
    }
}

class Real {
    constructor(real) {
        this.real = real;
    }

    // :: () -> false
    isNil() {
        return false;
    }

    value() {
        return this.real;
    }

    multiply(that) {
        if (that instanceof Int) {
            return new Real(this.real * that.int);
        }
        if (that instanceof Real) {
            return new Real(this.real * that.real);
        }
        throw new Error(`Not implemented ${this} * ${that}`);
    }

    divide(that) {
        if (that instanceof Int) {
            return new Real(this.real / that.int);
        }
        if (that instanceof Real) {
            return new Real(this.real / that.real);
        }
        throw new Error(`Not implemented ${this} / ${that}`);
    }

    add(that) {
        if (that instanceof Int) {
            return new Real(this.real + that.int);
        }
        if (that instanceof Real) {
            return new Real(this.real + that.real);
        }
        throw new Error(`Not implemented ${this} + ${that}`);
    }

    subtract(that) {
        if (that instanceof Int) {
            return new Real(this.real - that.int);
        }
        if (that instanceof Real) {
            return new Real(this.real - that.real);
        }
        throw new Error(`Not implemented ${this} - ${that}`);
    }

    equalTo(that) {
        if (that instanceof Int) {
            return new Bool(this.real === that.int);
        }
        if (that instanceof Real) {
            return new Bool(this.real === that.real);
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    lessThan(that) {
        if (that instanceof Int) {
            return new Bool(this.real < that.int);
        }
        if (that instanceof Real) {
            return new Bool(this.real < that.real);
        }
        throw new Error(`Not implemented ${this} < ${that}`);
    }

    toString() {
        if (Number.isInteger(this.real)) {
            return this.real + '.0';
        }
        return this.real.toString();
    }
}

class Str {
    constructor(str) {
        this.str = str;
    }

    // :: () -> false
    isNil() {
        return false;
    }

    value() {
        return this.str;
    }

    equalTo(that) {
        if (that instanceof Str) {
            return new Bool(this.str === that.str);
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    lessThan(that) {
        if (that instanceof Str) {
            return new Bool(this.str < that.str);
        }
        throw new Error(`Not implemented ${this} < ${that}`);
    }

    toString() {
        return `"${this.str}"`;
    }
}

class Sym {
    constructor(sym) {
        this.sym = sym.toUpperCase();
    }

    // :: () -> false
    isNil() {
        return false;
    }

    value() {
        return this.sym;
    }

    equalTo(that) {
        if (that instanceof Sym) {
            return new Bool(this.sym === that.sym);
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    toString() {
        return `'${this.sym}`;
    }
}

class List {
    constructor(arr) {
        // TODO: who should make copy, see cdr
        this.arr = [...arr];
    }

    // :: () -> true | false
    isNil() {
        return this.arr.length === 0;
    }

    // :: () -> Array
    value() {
        return this.arr;
    }

    // :: (List) -> Any
    cons(first) {
        return new List([first, ...this.arr]);
    }

    // :: (List) -> Any
    car() {
        return this.arr[0];
    }

    // :: (List) -> List
    cdr() {
        const [, ...rest] = this.arr;
        return new List(rest);
    }

    // :: (List) -> Integer
    length() {
        return this.arr.length;
    }

    // :: (List | Bool) -> Bool
    equalTo(that) {
        if (that instanceof List) {
            if (this.arr.length != that.arr.length) {
                return new Bool(false);
            }
            let result = new Bool(true);
            for (let i = 0; i < this.arr.length; i++) {
                result = result.and(this.arr[i].equalTo(that.arr[i]));
                if (result.isNil()) break;
            }
            return result;
        }
        if (that.isNil()) {
            return new Bool(this.isNil());
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    toString() {
        return `(${this.arr.join(' ')})`;
    }
}

class Pair {
    constructor(fst, snd) {
        this.fst = fst;
        this.snd = snd;
    }

    // :: () -> false
    isNil() {
        return false;
    }

    // :: (Pair) -> Any
    car() {
        return this.fst;
    }

    // :: (Pair) -> Any
    cdr() {
        return this.snd;
    }

    equalTo(that) {
        if (that instanceof Pair) {
            return this.fst.equalTo(that.fst).and(this.snd.equalTo(that.snd));
        }
        throw new Error(`Not implemented ${this} = ${that}`);
    }

    toString() {
        return `(${this.fst} . ${this.snd})`;
    }
}

class Fun {
    constructor(name, params, locals, fun) {
        this.name = name;
        this.params = params;
        this.locals = locals;
        this.fun = fun;
    }

    // :: () -> false
    isNil() {
        return false;
    }

    apply(evaluator, args) {
        return this.fun(evaluator, args);
    }

    toString() {
        return `(${this.name} ${params.join(' ')} ${locals.length > 0 ? ' / ' : ''} ${locals.join(' ')}})`;
    }
}

exports.Bool = Bool;
exports.Int = Int;
exports.Real = Real;
exports.Str = Str;
exports.Sym = Sym;
exports.List = List;
exports.Pair = Pair;
exports.Fun = Fun;

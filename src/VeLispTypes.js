/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2024 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

/* SPDX-License-Identifier: GPL-3.0-or-later */

import fs from 'fs'
import os from 'os'
import {find, escape} from './VeUtil.js'
import './VeJsExt.js' // Array.without
import {sleep} from './VeSystem.js'
import {KERNEL, LAMBDA} from './VeConst.js'

const TRU = 'T'
const NIL = 'nil'

class Bool {
    // :: (bool)
    constructor(bool) {
        this.bool = bool
    }

    // :: () -> Sym | Nil
    type() {
        if (this.bool) {
            return new Sym('sym')
        }
        return new Bool(false)
    }

    // :: () -> bool
    isNil() {
        return !this.bool
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    // :: () -> bool
    value() {
        return this.bool
    }

    // :: (Any) -> Bool
    and(that) {
        if (that instanceof Bool) {
            return new Bool(this.bool && that.bool)
        }
        throw new Error(`Not implemented (and ${this} ${that})`)
    }

    // :: (Any) -> Bool
    or(that) {
        if (that instanceof Bool) {
            return new Bool(this.bool || that.bool)
        }
        throw new Error(`Not implemented (or ${this} ${that})`)
    }

    // :: () -> Bool
    not() {
        return new Bool(!this.bool)
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Any) -> Bool
    lessThan(that) {
        if (that instanceof Bool) {
            return new Bool(this.bool < that.bool)
        }
        if (that instanceof List) {
            if (that.isNil()) {
                return new Bool(!this.isNil())
            }
        }
        return new Bool(true)
    }

    // :: (Any) -> Bool
    eq(that) {
        return this.equal(that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Bool) {
            return new Bool(this.bool === that.bool)
        }
        if (that instanceof List) {
            if (that.isNil()) {
                return new Bool(this.isNil())
            }
        }
        return new Bool(false)
    }

    // :: () -> string
    toString() {
        return this.bool ? TRU : NIL
    }
}

class Int {
    // :: (int)
    constructor(int) {
        this.int = int
    }

    // :: () -> Sym
    type() {
        return new Sym('int')
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    // :: () -> int
    value() {
        return this.int
    }

    multiply(that) {
        if (that instanceof Int) {
            return new Int(this.int * that.int)
        }
        if (that instanceof Real) {
            return new Real(this.int * that.real)
        }
        throw new Error(`Not implemented (* ${this} ${that})`)
    }

    divide(that) {
        if (that instanceof Int) {
            if (that.int === 0) {
                throw new Error('/: division by zero')
            }
            const res = this.int / that.int
            if (Number.isInteger(res)) {
                return new Int(res)
            }
            return new Int(Math.floor(res))
        }
        if (that instanceof Real) {
            if (that.real === 0) {
                throw new Error('/: division by zero')
            }
            return new Real(this.int / that.real)
        }
        throw new Error(`Not implemented (/ ${this} ${that})`)
    }

    // :: (Int | Real) -> Int | Real
    remainder(that) {
        if (that instanceof Int) {
            if (that.int === 0) {
                throw new Error('rem: division by zero')
            }
            return new Int(this.int % that.int)
        }
        if (that instanceof Real) {
            if (that.real === 0) {
                throw new Error('rem: division by zero')
            }
            return new Real(this.int % that.real)
        }
        throw new Error(`Not implemented (rem ${this} ${that})`)
    }

    add(that) {
        if (that instanceof Int) {
            return new Int(this.int + that.int)
        }
        if (that instanceof Real) {
            return new Real(this.int + that.real)
        }
        throw new Error(`Not implemented (+ ${this} ${that})`)
    }

    subtract(that) {
        if (that instanceof Int) {
            return new Int(this.int - that.int)
        }
        if (that instanceof Real) {
            return new Real(this.int - that.real)
        }
        throw new Error(`Not implemented (- ${this} ${that})`)
    }

    // :: () -> Int
    bitwiseNot() {
        return new Int(~this.int)
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Any) -> Bool
    lessThan(that) {
        if (that instanceof Int) {
            return new Bool(this.int < that.int)
        }
        if (that instanceof Real) {
            return new Bool(this.int < that.real)
        }
        throwTypeError('<:', that, [Int, Real])
    }

    // :: (Any) -> Bool
    eq(that) {
        return this.equal(that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Int) {
            return new Bool(this.int === that.int)
        }
        if (that instanceof Real) {
            return new Bool(this.int === that.real)
        }
        return new Bool(false)
    }

    // :: () -> string
    toString() {
        return this.int.toString()
    }
}

class Real {
    // (float | int)
    constructor(real) {
        this.real = real
    }

    // :: () -> Sym
    type() {
        return new Sym('real')
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    // :: () -> float
    value() {
        return this.real
    }

    multiply(that) {
        if (that instanceof Int) {
            return new Real(this.real * that.int)
        }
        if (that instanceof Real) {
            return new Real(this.real * that.real)
        }
        throw new Error(`Not implemented (* ${this} ${that})`)
    }

    divide(that) {
        if (that instanceof Int) {
            if (that.int === 0) {
                throw new Error('/: division by zero')
            }
            return new Real(this.real / that.int)
        }
        if (that instanceof Real) {
            if (that.real === 0.0) {
                throw new Error('/: division by zero')
            }
            return new Real(this.real / that.real)
        }
        throw new Error(`Not implemented (/ ${this} ${that})`)
    }

    // :: (Int | Real) -> Int | Real
    remainder(that) {
        if (that instanceof Int) {
            if (that.int === 0) {
                throw new Error('rem: division by zero')
            }
            return new Real(this.real % that.int)
        }
        if (that instanceof Real) {
            if (that.real === 0) {
                throw new Error('rem: division by zero')
            }
            return new Real(this.real % that.real)
        }
        throw new Error(`Not implemented (rem ${this} ${that})`)
    }

    add(that) {
        if (that instanceof Int) {
            return new Real(this.real + that.int)
        }
        if (that instanceof Real) {
            return new Real(this.real + that.real)
        }
        throw new Error(`Not implemented (+ ${this} ${that})`)
    }

    subtract(that) {
        if (that instanceof Int) {
            return new Real(this.real - that.int)
        }
        if (that instanceof Real) {
            return new Real(this.real - that.real)
        }
        throw new Error(`Not implemented (+ ${this} ${that})`)
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Any) -> Bool
    lessThan(that) {
        if (that instanceof Int) {
            return new Bool(this.real < that.int)
        }
        if (that instanceof Real) {
            return new Bool(this.real < that.real)
        }
        throwTypeError('<:', that, [Int, Real])
    }

    // :: (Any) -> Bool
    eq(that) {
        return this.equal(that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Int) {
            return new Bool(this.real === that.int)
        }
        if (that instanceof Real) {
            return new Bool(this.real === that.real)
        }
        return new Bool(false)
    }

    // :: () -> string
    toString() {
        if (Number.isInteger(this.real)) {
            return this.real + '.0'
        }
        return this.real.toString()
    }
}

class Str {
    // :: (string)
    constructor(str) {
        this.str = str
    }

    // :: () -> Sym
    type() {
        return new Sym('str')
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    // :: () -> string
    value() {
        return this.str
    }

    // :: () -> non_neg_int
    length() {
        return this.str.length
    }

    // :: (Str) -> Str
    concat(that) {
        if (that instanceof Str) {
            return new Str(this.str + that.str)
        }
        throw new Error(`Not implemented (strcat ${this} ${that})`)
    }

    // :: (Int, Int) -> Str
    substring(start, length) {
        return new Str(this.str.substring(start, start + length))
    }

    // :: () -> Str
    toUpperCase() {
        return new Str(this.str.toUpperCase())
    }

    // :: () -> Str
    toLowerCase() {
        return new Str(this.str.toLowerCase())
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Any) -> Bool
    lessThan(that) {
        if (that instanceof Str) {
            return new Bool(this.str < that.str)
        }
        throwTypeError('<:', that, [Str])
    }

    // :: (Any) -> Bool
    eq(that) {
        return this.equal(that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Str) {
            return new Bool(this.str === that.str)
        }
        return new Bool(false)
    }

    // :: () -> string
    toUnescapedString()  {
        return this.str
    }

    // :: () -> string
    toString() {
        return `"${escape(this.str)}"`
    }
}

class Sym {
    // :: (string)
    constructor(sym) {
        this.sym = sym.toUpperCase()
    }

    // :: () -> Sym
    type() {
        return new Sym('sym')
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    // :: () -> string
    value() {
        return this.sym
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Any) -> Bool
    eq(that) {
        return this.equal(that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Sym) {
            return new Bool(this.sym === that.sym)
        }
        return new Bool(false)
    }

    // :: () -> string
    toString() {
        return `${this.sym}`
    }
}

// TODO: consider re-implement List using Pair (Cons)
class List {
    // :: (Array)
    constructor(arr) {
        // TODO: who should make copy, see cdr
        this.arr = [...arr]
    }

    // :: () -> Sym | Nil
    type() {
        if (this.isNil()) {
            return new Bool(false)
        }
        return new Sym('list')
    }

    // :: () -> true | false
    isNil() {
        return this.arr.length === 0
    }

    // :: () -> bool
    isAtom() {
        return this.isNil()
    }

    // :: () -> Array
    value() {
        return this.arr
    }

    // :: (Any) -> List
    cons(first) {
        return new List([first, ...this.arr])
    }

    // :: (List) -> Any | Nil
    car() {
        if (this.arr.length > 0) {
            return this.arr[0]
        }
        return new Bool(false)
    }

    // :: (List) -> List | Nil
    cdr() {
        if (this.arr.length > 0) {
            const [, ...rest] = this.arr
            return new List(rest)
        }
        return new Bool(false)
    }

    // :: () -> non_neg_int
    length() {
        return this.arr.length
    }

    // :: (non_neg_int) -> Any
    at(index) {
        return this.arr[index]
    }

    // :: () -> Any
    last() {
        return this.arr[this.arr.length-1]
    }

    // :: (List) -> List
    concat(that) {
        if (that.isNil()) {
            return this
        }
        if (that instanceof List) {
            return new List(this.arr.concat(that.arr))
        }
        throw new Error(`Not implemented (append ${this} ${that})`)
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Any) -> Bool
    lessThan(that) {
        if (that instanceof List) {
            return new Bool(this.arr < that.arr)
        }
        throwTypeError('<:', that, [List])
    }

    // :: (Any) -> Bool
    eq(that) {
        if (that.isNil()) {
            return new Bool(this.isNil())
        }
        return new Bool(this === that)
    }

    // :: (List | Bool) -> Bool
    equal(that) {
        if (that instanceof List) {
            if (this.arr.length != that.arr.length) {
                return new Bool(false)
            }
            let result = new Bool(true)
            for (let i = 0; i < this.arr.length; i++) {
                result = result.and(this.arr[i].equal(that.arr[i]))
                if (result.isNil()) break
            }
            return result
        }
        if (that.isNil()) {
            return new Bool(this.isNil())
        }
        return new Bool(false)
    }

    // map :: ((AnyX) -> AnyY) -> List[AnyY]
    map(fn) {
        const arr = this.arr.map(fn)
        return new List(arr)
    }

    // :: () -> string
    toString() {
        if (this.isNil()) {
            return NIL
        }
        const arr = this.arr.map(item => item.toString())
        return `(${arr.join(' ')})`
    }
}

// TODO: consider re-implement List using Pair (Cons)
class Pair {
    // :: (Any, Any)
    constructor(fst, snd) {
        this.fst = fst
        this.snd = snd
    }

    // :: () -> Sym
    type() {
        return new Sym('list')
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return false
    }

    // :: (Any) -> List
    cons(first) {
        return new Pair(first, this)
    }

    // :: (Pair) -> Any
    car() {
        return this.fst
    }

    // :: (Pair) -> Any
    cdr() {
        return this.snd
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Pair) -> Bool
    lessThan(that) {
        if (that instanceof Pair) {
            return new Bool(
                this.fst < that.fst &&
                this.snd < that.snd
            )
        }
        throwTypeError('<:', that, [Pair])
    }

    // :: (Any) -> Bool
    eq(that) {
        return new Bool(this === that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Pair) {
            return this.fst.equal(that.fst).and(this.snd.equal(that.snd))
        }
        return new Bool(false)
    }

    toString() {
        let str = '('

        let cur = this
        for (;;) {
            str += cur.fst.toString()
            if (cur.snd.isNil()) {
                break
            }
            if (cur.snd.isAtom()) {
                str += ' . ' + cur.snd.toString()
                break
            }
            str += ' '
            cur = cur.snd
        }

        str += ')'
        return str
    }
}

// Abstract Fun - do NOT use directly
// KFun or UFun MUST be used instead
class Fun {
    // :: ( name   :: string
    //    , params :: [string]
    //    , locals :: [string]
    //    , fun    :: function
    //    , file   :: string | undefined
    //    ) -> Fun
    constructor(name, params, locals, fun, file = KERNEL) {
        this.name   = name
        this.params = params
        this.locals = locals
        this.fun    = fun
        this.file   = file
    }

    // :: () -> throw
    type() {
        throw new Error(`Not implemented for ${this.constructor.name}`)
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    apply(evaluator, args) {
        return this.fun(evaluator, args)
    }

    // :: (Any) -> Bool
    equalTo(that) {
        return this.eq(that)
    }

    // :: (Any) -> Bool
    eq(that) {
        return new Bool(this === that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Fun) {
            return new Bool(this.toString() === that.toString())
        }
        return new Bool(false)
    }

    // :: () -> string
    toString() {
        let prefix
        if (this.name !== LAMBDA) {
            prefix = `defun ${this.name}`
        } else {
            prefix = 'lambda'
        }
        const params = this.params.join(' ')
        const locals = this.locals.join(' ')
        return `(${prefix} (${params}${locals.length > 0 ? ' / ' : ''}${locals}))`
    }
}

// Kernel Function
class KFun extends Fun {
    // :: () -> Sym
    type() {
        return new Sym('subr')
    }
}

// User Function
class UFun extends Fun {
    // :: () -> Sym
    type() {
        return new Sym('usubr')
    }
}

const FileStream = {
    STDIN : 'stdin',
    STDOUT: 'stdout',
    STDERR: 'stderr'
}

const FileMode = {
    READ  : 'r',
    WRITE : 'w',
    APPEND: 'a'
}

const FileState = {
    OPEN  : 'o',
    CLOSED: 'c'
}

let stdinFile = undefined
let stdoutFile = undefined

class File {
    // :: (string, string, integer)
    /*private*/ constructor(name, mode, fd) {
        this.name = name
        this.mode = mode
        this.fd = fd
        this.state = FileState.OPEN
        this.buf = undefined
    }

    // :: (string, string) -> File
    static open(name, mode) {
        switch (name) {
        case FileStream.STDIN:
            if (!stdinFile) {
                stdinFile = new File(name, mode, process.stdin.fd)
            }
            return stdinFile

        case FileStream.STDOUT:
            if (!stdoutFile) {
                stdoutFile = new File(name, mode, process.stdout.fd)
            }
            return stdoutFile

        default:
            try {
                return new File(name, mode, fs.openSync(name, mode))
            } catch (e) {
                if (mode === FileMode.READ && e.code === 'ENOENT') {
                    return new Bool(false)
                }
                throw e
            }
        }
    }

    // :: () -> ()
    close() {
        if (this.state === FileState.CLOSED) {
            return new Bool(false)
        }
        switch (this.name) {
        case FileStream.STDIN:
            break
        case FileStream.STDOUT:
            break
        default:
            fs.closeSync(this.fd)
            this.fd = -1
            this.state = FileState.CLOSED
            break
        }
        return new Bool(false)
    }

    // :: ({echo: bool}) -> Int
    readChar({echo = false} = {}) {
        if (this.state === FileState.CLOSED || this.mode !== FileMode.READ) {
            throw new Error(`read-char: bad file ${this}`)
        }

        let buf = undefined

        if (this.buf) {
            buf = this.buf
            if (this.buf.length > 1) {
                this.buf = this.buf.subarray(1)
            } else {
                this.buf = undefined
            }
        } else {
            buf = Buffer.alloc(1)
            let len = 0
            // Loop while EAGAIN: resource TEMPORARILY unavailable
            for (;;) {
                try {
                    len = fs.readSync(this.fd, buf, 0, 1)
                    break
                } catch (e) {
                    if (e.code === 'EAGAIN') {
                        sleep(100)
                        continue
                    }
                    throw e
                }
            }
            if (!len) {
                return new Bool(false)
            }
        }

        if (echo) {
            fs.writeSync(process.stdout.fd, buf, 0, 1)
        }

        return new Int(buf[0])
    }

    // :: (Int) -> ()
    writeChar(num) {
        if (this.state === FileState.CLOSED || this.mode === FileMode.READ) {
            throw new Error(`write-char: bad file ${this}`)
        }
        const buf = Buffer.from([num.value()])
        fs.writeSync(this.fd, buf, 0, 1)
    }

    // :: ({eol: string, echo: bool}) -> Str
    readLine({eol = '\r\n', echo = false} = {}) {
        if (this.state === FileState.CLOSED || this.mode !== FileMode.READ) {
            throw new Error(`read-line: bad file ${this}`)
        }
        let stops = Array.from(eol).map(c => c.charCodeAt())
        let str = ''
        for (;;) {
            let buf = undefined

            if (this.buf) {
                buf = this.buf
                this.buf = undefined
            } else {
                buf = Buffer.alloc(32)
                let len = 0
                // Loop while EAGAIN: resource TEMPORARILY unavailable
                for (;;) {
                    try {
                        len = fs.readSync(this.fd, buf, 0, buf.length)
                        break
                    } catch (e) {
                        if (e.code === 'EAGAIN') {
                            sleep(100)
                            continue
                        }
                        throw e
                    }
                }
                if (!len) {
                    if (!str) {
                        return new Bool(false)
                    }
                    break
                }
                buf = buf.subarray(0, len)
            }

            let found = false
            let i = 0
            for (; i < buf.length; i++) {
                if (find(buf[i], stops)) {
                    str += buf.toString('utf8', 0, i)

                    // Drop once! stops
                    stops = stops.without(buf[i])
                    if (!stops.length) break

                    let j = i+1
                    for (; j < buf.length && stops.length > 0; j++) {
                        if (find(buf[j], stops)) {
                            stops = stops.without(buf[j])
                            continue
                        } else {
                            break
                        }
                    }
                    if (j < buf.length) {
                        this.buf = buf.subarray(j)
                    }
                    found = true
                    break
                }
            }

            if (found) {
                if (echo) {
                    fs.writeSync(process.stdout.fd, buf, 0, i)
                }

                break
            }

            str += buf.toString('utf8')

            if (echo) {
                fs.writeSync(process.stdout.fd, buf, 0, buf.length)
            }
        }
        if (echo) {
            fs.writeSync(process.stdout.fd, '\n')
        }
        return new Str(str)
    }

    // :: (Str) -> ()
    write(str) {
        if (this.state === FileState.CLOSED || this.mode === FileMode.READ) {
            throw new Error(`write: bad file ${this}`)
        }
        const buf = Buffer.from(str.value())
        fs.writeSync(this.fd, buf, 0, buf.length)
    }

    // :: (Str) -> ()
    writeLine(str) {
        if (this.state === FileState.CLOSED || this.mode === FileMode.READ) {
            throw new Error(`write-line: bad file ${this}`)
        }
        this.write(str)
        this.write(new Str(os.EOL))
    }

    // :: () -> Sym
    type() {
        return new Sym('file')
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    // :: (Any) -> Bool
    eq(that) {
        return new Bool(this === that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof File) {
            return new Bool(this === that)
        }
        return new Bool(false)
    }

    // :: () -> string
    toString() {
        return `#<file "${this.name}" ${this.mode}:${this.state}>`
    }
}

class Argv0 {
    // :: ([string])
    constructor(argv0) {
        this.argv0 = [...argv0]
    }

    // :: () -> Sym
    type() {
        return new Sym('argv0')
    }

    // :: () -> false
    isNil() {
        return false
    }

    // :: () -> bool
    isAtom() {
        return true
    }

    // :: () -> [string]
    value() {
        return this.argv0
    }

    // :: (Any) -> Bool
    eq(that) {
        return new Bool(this === that)
    }

    // :: (Any) -> Bool
    equal(that) {
        if (that instanceof Argv0) {
            return new Bool(this === that)
        }
        return new Bool(false)
    }

    // :: () -> string
    toString() {
        // Use `.inspect (argv 0)` to look inside
        return '#<argv0>'
    }
}

function ensureType(prefix, argValue, argTypes) {
    for (const argType of argTypes) {
        if (argValue instanceof argType) {
            return argValue
        }
    }
    throwTypeError(prefix, argValue, argTypes)
}

function throwTypeError(prefix, argValue, argTypes) {
    const typeNames = argTypes.map(type => type.name).join(', ')
    throw new Error(
        `${prefix} expected ${typeNames}`//, but saw ${argValue.constructor.name}: ${argValue}`
    )
}

export {
    Bool,
    Int,
    Real,
    Str,
    Sym,
    List,
    Pair,
    Fun,
    KFun,
    UFun,
    FileStream,
    FileMode,
    File,
    Argv0,
    ensureType,
    throwTypeError,
}

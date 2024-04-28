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

import {Bool, Int, Str, Sym, List, KFun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('ATOMS-FAMILY', new KFun('atoms-family', ['format', '[symlist]'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('atoms-family: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('atoms-family: too many arguments')
        }
        const SYM_LIST = 0
        const STR_LIST = 1
        let format = ensureType('atoms-family: `format`', args[0], [Int]).value()
        if (format < SYM_LIST) {
            throw new Error('atoms-family: `format` expected non-negative Int')
        }
        if (format > STR_LIST) {
            format = STR_LIST
        }
        let symlist = []
        if (args.length === 2) {
            const arg = args[1]
            if (arg.isNil()) {
                return new List([])
            }
            if (! (arg instanceof List)) {
                throw new Error('atoms-family: `symlist` expected List')
            }
            const list = ensureType('atoms-family: `symlist`', args[1], [List]).value()
            symlist = list.map(s => {
                if (s instanceof Str) {
                    return s.value().toUpperCase()
                }
                throw new Error('atoms-family: `symlist` expected List of Str')
            })
        }
        let syms = self.stack.top().getSyms()
        if (symlist.length) {
            syms = symlist.map(s => syms.indexOf(s) !== -1 ? s : null)
        }
        if (format === SYM_LIST) {
            return new List(syms.map(s => s ? new Sym(s) : new Bool(false)))
        }
        return new List(syms.map(s => s ? new Str(s) : new Bool(false)))
    }))
    context.setSym('BOUNDP', new KFun('boundp', ['sym'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('boundp: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('boundp: too many arguments')
        }
        const name = args[0] // Allow everything
        const value = self.stack.top().getVar(name)
        if (value instanceof Bool && value.value() === false) {
            // sym is undefined, but what's the point to automatically create it?
            return new Bool(false)
        }
        return new Bool(true)
    }))
    context.setSym('SET', new KFun('set', ['sym', 'expr'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('set: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('set: too many arguments')
        }
        const name = ensureType('set: `sym`', args[0], [Sym])
        const value = args[1]
        //console.error(`set: ${name} = ${value}`);
        self.stack.top().setVar(name, value)
        return value
    }))
    context.setSym('TYPE', new KFun('type', ['item'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('type: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('type: too many arguments')
        }
        return args[0].type()
    }))
}

/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2023 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

import {Bool, List, Pair, Sym, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('APPEND', new Fun('append', ['[list ...]'], [], (self, args) => {
        if (args.length === 0) {
            return new Bool(false)
        }
        let result = new List([])
        for (let i = 0; i < args.length; i++) {
            if (args[i].isNil()) {
                continue
            }
            const list = ensureType('append:', args[i], [List])
            result = result.concat(list)
        }
        return result
    }))
    context.setSym('CAR', new Fun('car', ['listorpair'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('car: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('car: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof List || arg instanceof Pair) {
            return arg.car()
        }
        if (arg.isNil()) {
            return new Bool(false)
        }
        throw new Error('car: expected List, Pair')
    }))
    context.setSym('CDR', new Fun('cdr', ['listorpair'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('cdr: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('cdr: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof List || arg instanceof Pair) {
            return arg.cdr()
        }
        if (arg.isNil()) {
            return new Bool(false)
        }
        throw new Error('cdr: expected List, Pair')
    }))
    context.setSym('CONS', new Fun('cons', ['first', 'listoratom'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('cons: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('cons: too many arguments')
        }
        const fst = args[0]
        const snd = args[1]
        if (snd instanceof List || snd instanceof Pair) {
            return snd.cons(fst)
        } else if (snd.isNil()) {
            return new List([fst])
        } else {
            return new Pair(fst, snd)
        }
    }))
    context.setSym('LIST', new Fun('list', ['[expr ...]'], [], (self, args) => {
        const result = []
        for (let i = 0; i < args.length; i++) {
            result.push(args[i])
        }
        return new List(result)
    }))
    context.setSym('MAPCAR', new Fun('mapcar', ['function list [list ...]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('mapcar: too few arguments')
        }
        let fun = args[0]
        if (fun instanceof Sym) {
            // Try resolving symbol to function
            fun = self.contexts[self.contexts.length-1].getSym(fun.value())
        }
        if (fun instanceof Fun) {
            // Prepare lists
            const lists = []
            let minLen = null
            for (let i = 1; i < args.length; i++) {
                // If any list is nil, return empty list immediately,
                // since minLen == 0 anyway
                if (args[i].isNil()) {
                    return new List([])
                }
                const list = ensureType('mapcar: `list`', args[i], [List])
                if (!minLen) {
                    minLen = list.length()
                } else {
                    minLen = Math.min(minLen, list.length())
                }
                lists.push(list)
            }
            // Map lists
            const result = []
            for (let i = 0; i < minLen; i++) {
                const vector = []
                for (let j = 0; j < lists.length; j++) {
                    vector.push(lists[j].arr[i])
                }
                result.push(fun.apply(self, vector))
            }
            return new List(result)
        }
        throw new Error(`mapcar: no such function ${args[0]}`)
    }))
}

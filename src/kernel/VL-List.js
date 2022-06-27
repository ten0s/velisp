/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

import {Bool, Int, Sym, List, Pair, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('VL-CONSP', new Fun('vl-consp', ['list'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-consp: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-consp: too many arguments')
        }
        const arg = args[0]
        if (arg.isNil()) {
            return new Bool(false)
        }
        if (arg instanceof List || arg instanceof Pair) {
            return new Bool(true)
        }
        return new Bool(false)
    }))
    context.setSym('VL-EVERY', new Fun('vl-every', ['predicate', 'list', '[list ...]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('vl-every: too few arguments')
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
                    return new Bool(true)
                }
                const list = ensureType('vl-every: `list`', args[i], [List])
                if (!minLen) {
                    minLen = list.length()
                } else {
                    minLen = Math.min(minLen, list.length())
                }
                lists.push(list)
            }
            // Process lists
            for (let i = 0; i < minLen; i++) {
                const vector = []
                for (let j = 0; j < lists.length; j++) {
                    vector.push(lists[j].arr[i])
                }
                if (fun.apply(self, vector).isNil()) {
                    return new Bool(false)
                }
            }
            return new Bool(true)
        }
        throw new Error(`vl-every: \`predicate\` no such function ${args[0]}`)
    }))
    context.setSym('VL-SOME', new Fun('vl-some', ['predicate', 'list', '[list ...]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('vl-some: too few arguments')
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
                    return new Bool(false)
                }
                const list = ensureType('vl-some: `list`', args[i], [List])
                if (!minLen) {
                    minLen = list.length()
                } else {
                    minLen = Math.min(minLen, list.length())
                }
                lists.push(list)
            }
            // Process lists
            for (let i = 0; i < minLen; i++) {
                const vector = []
                for (let j = 0; j < lists.length; j++) {
                    vector.push(lists[j].arr[i])
                }
                if (!fun.apply(self, vector).isNil()) {
                    return new Bool(true)
                }
            }
            return new Bool(false)
        }
        throw new Error(`vl-some: \`predicate\` no such function ${args[0]}`)
    }))
    context.setSym('VL-LIST*', new Fun('vl-list*', ['object', '[object ...]'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('vl-list*: too few arguments')
        }
        if (args.length === 1) {
            return args[0]
        }
        let result = undefined
        let last = args[args.length-1]
        let prelast = args[args.length-2]
        if (last.isNil()) {
            result = new List([prelast])
        }
        if (last instanceof List || last instanceof Pair) {
            result = last.cons(prelast)
        } else {
            result = new Pair(prelast, last)
        }
        for (let i = args.length-3; i >= 0; i--) {
            result = result.cons(args[i])
        }
        return result
    }))
    context.setSym('VL-LIST-LENGTH', new Fun('vl-list-length', ['list-or-cons-object'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('vl-list-length: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-list-length: too many arguments')
        }
        const arg = args[0]
        if (arg.isNil()) {
            return new Int(0)
        }
        if (arg instanceof List) {
            if (arg.last() instanceof Pair) {
                return new Bool(false)
            }
            return new Int(arg.length())
        }
        if (arg instanceof Pair) {
            return new Bool(false)
        }
        ensureType('vl-list-length:', arg, [List, Pair])
    }))
}

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

import {Bool, Str, Sym, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('VL-SYMBOL-NAME', new Fun('vl-symbol-name', ['sym'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-symbol-name: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-symbol-name: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof Sym) {
            return new Str(arg.toString())
        }
        // Handle T
        if (arg instanceof Bool && arg.value()) {
            return new Str(arg.toString())
        }
        ensureType('vl-symbol-name:', arg, [Sym])
    }))
    context.setSym('VL-SYMBOL-VALUE', new Fun('vl-symbol-value', ['sym'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-symbol-value: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-symbol-value: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof Sym) {
            return self.contexts[self.contexts.length-1].getSym(arg.value())
        }
        // Handle T
        if (arg instanceof Bool && arg.value()) {
            return arg
        }
        ensureType('vl-symbol-value:', arg, [Sym])
    }))
    context.setSym('VL-SYMBOLP', new Fun('vl-symbolp', ['obj'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('vl-symbolp: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('vl-symbolp: too many arguments')
        }
        const arg = args[0]
        if (arg instanceof Sym) {
            return new Bool(true)
        }
        // Handle T
        if (arg instanceof Bool && arg.value()) {
            return arg
        }
        return new Bool(false)
    }))
}

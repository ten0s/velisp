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

import {Bool, Sym, Fun, KFun} from '../VeLispTypes.js'

const _traces = new Set()

export const isTrace = (fun) => {
    return _traces.has(fun)
}

export const initContext = (context) => {
    context.setSym('TRACE', new KFun('trace', ['[function] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Bool(false)
        }
        let fun = new Bool(false)
        for (let i = 0; i < args.length; i++) {
            fun = args[i]
            if (fun instanceof Sym) {
                // Try resolving symbol to function
                fun = self.contexts.top().getSym(fun.value())
            }
            if (fun instanceof Fun) {
                _traces.add(fun)
                continue
            }
            if (!fun.isNil()) {
                throw new Error('trace: `function` expected Sym, Fun')
            }
        }
        if (fun.isNil()) {
            return fun
        }
        return new Sym(fun.name)
    }))
    context.setSym('UNTRACE', new KFun('untrace', ['[function] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Bool(false)
        }
        let fun = new Bool(false)
        for (let i = 0; i < args.length; i++) {
            fun = args[i]
            if (fun instanceof Sym) {
                // Try resolving symbol to function
                fun = self.contexts.top().getSym(fun.value())
            }
            if (fun instanceof Fun) {
                _traces.delete(fun)
                continue
            }
            if (!fun.isNil()) {
                throw new Error('untrace: `function` expected Sym, Fun')
            }
        }
        if (fun.isNil()) {
            return fun
        }
        return new Sym(fun.name)
    }))
}

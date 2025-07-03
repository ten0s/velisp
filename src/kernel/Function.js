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

import {Bool, Int, Real, Str, Sym, List, Fun, KFun} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('APPLY', new KFun('apply', ['function', 'list'], [], (self, args) => {
        //console.log('apply args', args);
        if (args.length < 2) {
            throw new Error('apply: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('apply: too many arguments')
        }
        let fun = args[0]
        if (fun instanceof Sym) {
            // Try resolving symbol to function
            fun = self.stack.top().getSym(fun.value())
        }
        if (fun instanceof Fun) {
            let list = args[1]
            if (list instanceof List) {
                list = list.value()
            } else if (list.isNil()) {
                list = []
            } else {
                throw new Error('apply: `list` expected List')
            }
            return fun.apply(self, list)
        }
        throw new Error(`apply: no such function ${args[0]}`)
    }))
    context.setSym('EVAL', new KFun('eval', ['expr'], [], (self, args) => {
        //console.log('eval args', args)

        if (args.length < 1) {
            throw new Error('eval: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('eval: too many arguments')
        }
        return eval_expr(self, args[0])
    }))
}

function eval_expr(self, expr) {
    if (expr instanceof Bool) {
        return expr
    }
    if (expr instanceof Int) {
        return expr
    }
    if (expr instanceof Real) {
        return expr
    }
    if (expr instanceof Str) {
        return expr
    }
    if (expr instanceof Sym) {
        // Resolve symbol, nil if not found
        return self.stack.top().getSym(expr.value())
    }
    if (expr instanceof Fun) {
        return expr
    }
    if (expr instanceof List) {
        if (expr.isNil()) {
            return expr
        }
        let car = expr.car()
        let fun = car
        if (fun instanceof Sym) {
            // Try resolving symbol to function
            fun = self.stack.top().getSym(fun.value())
        }
        if (fun instanceof Fun) {
            let list = expr.cdr().value()
            return fun.apply(self, list)
        }
        throw new Error(`eval: no such function ${car}`)
    }
    throw new Error('eval: evaluation failed')
}

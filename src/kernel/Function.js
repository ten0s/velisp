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

const specialForms = {
    'AND': eval_and,
//    'COND': eval_cond,
//    'DEFUN': eval_defun,
//    'FOREACH': eval_foreach,
//    'FUNCTION': eval_function,
    'IF': eval_if,
//    'LAMBDA': eval_lambda,
    'OR': eval_or,
//    'PROGN': eval_progn,
//    'QUOTE': eval_quote,
//    'REPEAT': eval_repeat,
//    'SETQ': eval_setq,
//    'WHILE': eval_while,
//    'TICK': eval_tick
}

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
        if (specialForms.hasOwnProperty(expr.value())) {
            return expr
        }
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

        let head = expr.car()
        let tail = expr.cdr()

        if (!head.isAtom()) {
            head = eval_expr(self, head)
        }

        // Evaluate special form expressions
        const specFun = specialForms[head.value()]
        if (specFun) {
            return specFun(self, tail)
        }

        // Evaluate internal expressions
        // List[AnyX] -> List[AnyY]
        tail = tail.map(X => {
            return eval_expr(self, X)
        })
        expr = tail.cons(head)

        const car = expr.car()
        let fun = car
        if (fun instanceof Sym) {
            // Try resolving symbol to function
            fun = self.stack.top().getSym(fun.value())
        }
        if (fun instanceof Fun) {
            const args = expr.cdr().value()
            return fun.apply(self, args)
        }
        throw new Error(`eval: no such function ${car}`)
    }
    throw new Error('eval: evaluation failed')
}

function eval_and(self, args) {
    for (let i = 0; i < args.length(); i++) {
        const result = eval_expr(self, args.at(i))
        if (result.isNil()) {
            return new Bool(false)
        }
    }
    return new Bool(true)
}

function eval_if(self, args) {
    const test = eval_expr(self, args.at(0))
    //console.error('if test:', test)
    if (!test.isNil()) {
        return eval_expr(self, args.at(1))
    }
    if (args.length() > 2) {
        return eval_expr(self, args.at(2))
    }
    return new Bool(false)
}

function eval_or(self, args) {
    for (let i = 0; i < args.length(); i++) {
        const result = eval_expr(self, args.at(i))
        if (!result.isNil()) {
            return new Bool(true)
        }
    }
    return new Bool(false)
}

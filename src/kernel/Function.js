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

import {evaluate} from '../VeLispEvaluator.js'
import {Bool, Int, Real, Str, Sym, List, Fun, KFun} from '../VeLispTypes.js'

const specialForms = {
    'AND': evalAnd,
    'COND': evalCond,
    'DEFUN': evalDefun,
    'FOREACH': evalForeach,
    'FUNCTION': evalFunction,
    'IF': evalIf,
    'LAMBDA': evalLambda,
    'OR': evalOr,
    'PROGN': evalProgn,
    'QUOTE': evalQuote,
    'REPEAT': evalRepeat,
    'SETQ': evalSetQ,
    'WHILE': evalWhile
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
        return evalExpr(self, args[0])
    }))
}

function evalExpr(self, expr, resolveSym = true) {
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
        if (resolveSym) {
            if (specialForms.hasOwnProperty(expr.value())) {
                return expr
            }
            // Resolve symbol, nil if not found
            return self.stack.top().getSym(expr.value())
        }
        return expr
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
            head = evalExpr(self, head)
        }

        // Evaluate special form expressions
        if (!(head instanceof Fun)) {
            const specFun = specialForms[head.value()]
            if (specFun) {
                return specFun(self, tail)
            }
        }

        // Evaluate internal expressions
        // List[AnyX] -> List[AnyY]
        tail = tail.map(X => evalExpr(self, X))
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

function evalAnd(self, args) {
    for (let i = 0; i < args.length(); i++) {
        const result = evalExpr(self, args.at(i))
        if (result.isNil()) {
            return new Bool(false)
        }
    }
    return new Bool(true)
}

function evalCond(self, args) {
    let result = new Bool(false)
    for (let i = 0; i < args.length(); i++) {
        const clause = args.at(i)
        const test = evalExpr(self, clause.car())
        //console.error('cond test:', test)
        if (!test.isNil()) {
            result = test
            const body = clause.cdr()
            //console.error('cond body: ', body)
            for (let j = 0; j < body.length(); j++) {
                result = evalExpr(self, body.at(j))
            }
            break
        }
    }
    return result
}

function evalDefun(self, args) {
    // Restore defun's string representation and evaluate it.
    const defun = args.cons(new Sym('defun')).toString()
    return evaluate(defun, self.stack)
}

function evalForeach(self, args) {
    const name = args.at(0)
    const list = evalExpr(self, args.at(1))

    //console.error(`foreach: ${name} ${list}`)
    if (list.isNil()) {
        return new Bool(false)
    }

    if (list instanceof List) {
        // Check if body exists
        if (args.length() === 2) {
            return new Bool(false)
        }

        let result = new Bool(false)

        const context = self.stack.top()

        // Save previous 'name' from the top context, if defined
        let prevVar = undefined
        if (context.isTopVar(name)) {
            prevVar = context.getTopVar(name)
        }

        for (let i = 0; i < list.length(); i++) {
            // Set each list value directly into the top context,
            // to make it possible to shadow 'name' in some
            // lower context
            const value = list.at(i)
            //console.error(`foreach: ${value}`)
            context.setTopVar(name, value)
            const body = args.cdr().cdr()
            for (let j = 0; j < body.length(); j++) {
                result = evalExpr(self, body.at(j))
            }
        }

        // Restore 'name' if was previously defined in the top context
        // or delete it otherwise
        if (prevVar) {
            context.setTopVar(name, prevVar)
        } else {
            context.delTopVar(name)
        }

        return result
    }

    throw new Error('eval: foreach: `list` expected List')
}

function evalFunction(self, args) {
    const arg = args.car()
    if (arg instanceof Sym) {
        return arg
    }
    if (arg instanceof List && !arg.car().equal(new Sym('lambda')).isNil()) {
        // Get rid of initial Sym('lambda') and evaluate
        return evalLambda(self, arg.cdr())
    }
    if (arg instanceof List && !arg.car().equal(new Sym('defun')).isNil()) {
        // Get rid of initial Sym('defun') and evaluate
        return evalDefun(self, arg.cdr())
    }
    throw new Error('eval: function: expected Sym, Fun')
}

function evalIf(self, args) {
    const test = evalExpr(self, args.at(0))
    if (!test.isNil()) {
        return evalExpr(self, args.at(1))
    }
    if (args.length() > 2) {
        return evalExpr(self, args.at(2))
    }
    return new Bool(false)
}

function evalLambda(self, args) {
    // Restore lambda's string representation and evaluate it.
    const lambda = args.cons(new Sym('lambda')).toString()
    return evaluate(lambda, self.stack)
}

function evalOr(self, args) {
    for (let i = 0; i < args.length(); i++) {
        const result = evalExpr(self, args.at(i))
        if (!result.isNil()) {
            return new Bool(true)
        }
    }
    return new Bool(false)
}

function evalProgn(self, args) {
    let result = new Bool(false)
    for (let i = 0; i < args.length(); i++) {
        result = evalExpr(self, args.at(i))
    }
    return result
}

function evalQuote(self, args) {
    // Simply return the first arg
    return args.car()
}

function evalRepeat(self, args) {
    const count = evalExpr(self, args.car())
    if (count instanceof Int && count.value() >= 0) {
        let result = new Bool(false)
        const body = args.cdr()
        for (let i = 0; i < count.value(); i++) {
            for (let j = 0; j < body.length(); j++) {
                result = evalExpr(self, body.at(j))
            }
        }
        return result
    }
    throw new Error('eval: repeat: `num` expected non-negative Int')
}

function evalSetQ(self, args) {
    let value = new Bool(false)
    for (let i = 0; i < args.length(); i+=2) {
        const name = evalExpr(self, args.at(i), false)
        value = evalExpr(self, args.at(i+1))
        //console.error(`setq: ${name} = ${value}`)
        self.stack.top().setVar(name, value)
    }
    return value
}

function evalWhile(self, args) {
    let result = new Bool(false)
    const test = args.car()
    const body = args.cdr()
    for (;;) {
        if (evalExpr(self, test).isNil()) {
            break
        }
        for (let i = 0; i < body.length(); i++) {
            result = evalExpr(self, body.at(i))
        }
    }
    return result
}

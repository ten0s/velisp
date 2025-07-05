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

import VeLispParser from '../grammar/VeLispParser.js'
import VeLispVisitor from '../grammar/VeLispVisitor.js'
import {unescape} from './VeUtil.js'
import {Bool, Int, Real, Str, Sym, List, Pair} from './VeLispTypes.js'

class VeLispReadVisitor extends VeLispVisitor {
    constructor() {
        super()
    }

    visitAnd(ctx) {
        const values = [
            new Sym('and')
        ]

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitCond(ctx) {
        const values = [
            new Sym('cond')
        ]

        for (let i = 0; i < ctx.condTestResult().length; i++) {
            const clause = []
            const test = this.getValue(this.visit(ctx.condTestResult(i).condTest()))
            clause.push(test)
            for (let j = 0; j < ctx.condTestResult(i).condResult().length; j++) {
                const result = this.getValue(this.visit(ctx.condTestResult(i).condResult(j)))
                clause.push(result)
            }
            values.push(new List(clause))
        }

        return new List(values)
    }

    visitDefun(ctx) {
        const values = [
            new Sym('defun'),
            new Sym(this.visit(ctx.funName().ID()))
        ]

        values.push(new Sym('('))

        for (let i = 0; i < ctx.funParam().length; i++) {
            const param = new Sym(this.visit(ctx.funParam(i).ID()))
            values.push(param)
        }

        if (ctx.funLocal().length) {
            values.push(new Sym('/'))

            for (let i = 0; i < ctx.funLocal().length; i++) {
                const param = new Sym(this.visit(ctx.funLocal(i).ID()))
                values.push(param)
            }
        }

        values.push(new Sym(')'))

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitForeach(ctx) {
        const values = [
            new Sym('foreach'),
            new Sym(this.visit(ctx.foreachName().ID())),
            this.getValue(this.visit(ctx.foreachList()))
        ]

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitFunction(ctx) {
        const values = [
            new Sym('function'),
            this.getValue(this.visit(ctx.expr()))
        ]

        return new List(values)
    }

    visitIf(ctx) {
        const values = [
            new Sym('if'),
            this.getValue(this.visit(ctx.ifTest())),
            this.getValue(this.visit(ctx.ifThen()))
        ]

        if (ctx.ifElse()) {
            values.push(this.getValue(this.visit(ctx.ifElse())))
        }

        return new List(values)
    }

    visitLambda(ctx) {
        const values = [
            new Sym('lambda')
        ]

        values.push(new Sym('('))

        for (let i = 0; i < ctx.funParam().length; i++) {
            const param = new Sym(this.visit(ctx.funParam(i).ID()))
            values.push(param)
        }

        if (ctx.funLocal().length) {
            values.push(new Sym('/'))

            for (let i = 0; i < ctx.funLocal().length; i++) {
                const param = new Sym(this.visit(ctx.funLocal(i).ID()))
                values.push(param)
            }
        }

        values.push(new Sym(')'))

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitOr(ctx) {
        const values = [
            new Sym('or')
        ]

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitProgn(ctx) {
        const values = [
            new Sym('progn')
        ]

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitQuote(ctx) {
        const values = [
            new Sym('quote'),
            this.getValue(this.visit(ctx.expr()))
        ]

        return new List(values)
    }

    visitRepeat(ctx) {
        const values = [
            new Sym('repeat'),
            this.getValue(this.visit(ctx.repeatNum()))
        ]

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitSetQ(ctx) {
        const values = [
            new Sym('setq')
        ]

        for (let i = 0; i < ctx.setqNameExpr().length; i++) {
            const name = new Sym(this.visit(ctx.setqNameExpr(i).ID()))
            const value = this.getValue(this.visit(ctx.setqNameExpr(i).expr()))
            values.push(name)
            values.push(value)
        }

        return new List(values)
    }

    visitWhile(ctx) {
        const values = [
            new Sym('while'),
            this.getValue(this.visit(ctx.whileTest())),
        ]

        for (let i = 0; i < ctx.expr().length; i++) {
            const value = this.getValue(this.visit(ctx.expr(i)))
            values.push(value)
        }

        return new List(values)
    }

    visitTick(ctx) {
        return this.visitQuote(ctx)
    }

    visitDotList(ctx) {
        const length = ctx.listExpr().length
        let last = this.getValue(this.visit(ctx.listExpr(length-1)))
        //console.error(last)
        if (last.isNil()) {
            last = new List([])
        }
        if (last instanceof List) {
            for (let i = length - 2; i >= 0; i--) {
                const value = this.getValue(this.visit(ctx.listExpr(i)))
                last = last.cons(value)
            }
            return last
        }
        const prelast = this.getValue(this.visit(ctx.listExpr(length-2)))
        let result = new Pair(prelast, last)
        for (let i = length - 3; i >= 0; i--) {
            const value = this.getValue(this.visit(ctx.listExpr(i)))
            result = result.cons(value)
        }
        return result
    }

    visitList(ctx) {
        const length = ctx.listExpr().length

        if (length === 0) {
            return new Bool(false)
        }

        const values = []
        for (let i = 0; i < length; i++) {
            const value = this.getValue(this.visit(ctx.listExpr(i).expr()))
            values.push(value)
        }

        return new List(values)
    }

    visitTerminal(ctx) {
        const str = ctx.getText()
        if (ctx.parentCtx instanceof VeLispParser.NilContext) {
            //console.error('NIL:', str)
            return new Bool(false)
        }
        if (ctx.parentCtx instanceof VeLispParser.TruContext) {
            //console.error('T:', str)
            return new Bool(true)
        }
        if (ctx.parentCtx instanceof VeLispParser.IntContext) {
            //console.error('INT:', str)
            return new Int(Number.parseInt(str))
        }
        if (ctx.parentCtx instanceof VeLispParser.RealContext) {
            //console.error('REAL:', str)
            return new Real(Number.parseFloat(str))
        }
        if (ctx.parentCtx instanceof VeLispParser.StrContext) {
            //console.error('STR:', str)
            // Remove first and last double quotes (")
            return new Str(unescape(str.substring(1, str.length-1)))
        }
        if (ctx.parentCtx instanceof VeLispParser.IdContext) {
            //console.error('ID:', str)
            return new Sym(str)
        }

        // Also handles ID outside of expr
        //console.error('TERMINAL:', str)
        //console.error(ctx)
        return str
    }

    getValue(expr) {
        if (expr instanceof Array) {
            return this.getValue(expr[0])
        }
        return expr
    }
}

export default VeLispReadVisitor

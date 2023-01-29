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

import VeLispParser from '../grammar/VeLispParser.js'
import VeLispVisitor from '../grammar/VeLispVisitor.js'
import VeLispContext from './VeLispContext.js'
import {unescape} from './VeUtil.js'
import {Bool, Int, Real, Str, Sym, List, Pair, Fun, UFun} from './VeLispTypes.js'
import {isTrace} from './kernel/Trace.js'

class VeLispEvalVisitor extends VeLispVisitor {
    constructor(stack) {
        super()
        this.stack = stack
        this.traceDepth = 0
    }

    visitAnd(ctx) {
        for (let i = 0; i < ctx.expr().length; i++) {
            const result = this.getValue(this.visit(ctx.expr(i)))
            if (result.isNil()) {
                return new Bool(false)
            }
        }
        return new Bool(true)
    }

    visitCond(ctx) {
        let result = new Bool(false)
        for (let i = 0; i < ctx.condTestResult().length; i++) {
            const test = this.getValue(this.visit(ctx.condTestResult(i).condTest()))
            //console.error('cond test:', test);
            if (!test.isNil()) {
                result = test
                for (let j = 0; j < ctx.condTestResult(i).condResult().length; j++) {
                    result = this.getValue(this.visit(ctx.condTestResult(i).condResult(j)))
                }
                break
            }
        }
        return result
    }

    visitDefun(ctx) {
        const name = this.visit(ctx.funName().ID()).toUpperCase()
        const fun = this.makeUFun(name, ctx)
        this.stack.top().setSym(name, fun)
        return new Sym(name)
    }

    visitForeach(ctx) {
        const name = this.visit(ctx.foreachName().ID()).toUpperCase()
        const list = this.getValue(this.visit(ctx.foreachList()))

        const line = ctx.start.line
        this.stack.top().callerLine = line

        //console.error(`foreach: ${name} ${list}`);
        if (list.isNil()) {
            return new Bool(false)
        }
        if (list instanceof List) {
            let result = new Bool(false)
            // Push new context with 'name' = nil
            const context = new VeLispContext(this.stack.top())
            context.initVar(name, new Bool(false))
            this.stack.push(context)
            for (let i = 0; i < list.value().length; i++) {
                // Set each list value directly into our context, not the top context,
                // to make it possible to shadow 'name' in some upper context
                const value = list.value()[i]
                //console.error(`foreach: ${value}`);
                context.setVar(name, value)
                for (let j = 0; j < ctx.expr().length; j++) {
                    result = this.visit(ctx.expr(j))
                }
            }
            // Pop new context
            this.stack.pop()
            return result
        }
        throw new Error('foreach: `list` expected List')
    }

    visitFunction(ctx) {
        const expr = ctx.expr()
        const str = expr.getText()

        const line = ctx.start.line
        this.stack.top().callerLine = line

        if (expr instanceof VeLispParser.IdContext) {
            //console.error('ID:', str);
            return new Sym(str)
        }
        if (expr instanceof VeLispParser.LambdaContext) {
            return this.visitLambda(expr)
        }
        //console.error(str);
        //console.error(ctx.expr());

        throw new Error('function: expected Fun')
    }

    visitIf(ctx) {
        const test = this.getValue(this.visit(ctx.ifTest()))
        //console.error('if test:', test);
        if (!test.isNil()) {
            return this.visit(ctx.ifThen())
        } else {
            if (ctx.ifElse()) {
                return this.visit(ctx.ifElse())
            } else {
                return new Bool(false)
            }
        }
    }

    visitLambda(ctx) {
        return this.makeUFun('#<lambda>', ctx)
    }

    visitOr(ctx) {
        for (let i = 0; i < ctx.expr().length; i++) {
            const result = this.getValue(this.visit(ctx.expr(i)))
            if (!result.isNil()) {
                return new Bool(true)
            }
        }
        return new Bool(false)
    }

    visitProgn(ctx) {
        let result = new Bool(false)
        for (let i = 0; i < ctx.expr().length; i++) {
            result = this.visit(ctx.expr(i))
        }
        return result
    }

    visitQuote(ctx) {
        const expr = ctx.expr()
        const str = expr.getText()

        const line = ctx.start.line
        this.stack.top().callerLine = line

        if (expr instanceof VeLispParser.NilContext) {
            //console.error('NIL:', str);
            return new Bool(false)
        } else if (expr instanceof VeLispParser.IntContext) {
            //console.error('INT:', str);
            return new Int(Number.parseInt(str))
        } else if (expr instanceof VeLispParser.RealContext) {
            //console.error('REAL:', str);
            return new Real(Number.parseFloat(str))
        } else if (expr instanceof VeLispParser.StrContext) {
            //console.error('STR:', str);
            // Remove first and last double quotes (")
            return new Str(unescape(str.substring(1, str.length-1)))
        } else if (expr instanceof VeLispParser.IdContext) {
            //console.error('ID:', str);
            return new Sym(str)
        } else if (expr instanceof VeLispParser.DotListContext) {
            //console.error('DOTLIST:', str);
            const length = expr.listExpr().length
            let last = this.getValue(this.visitQuote(expr.listExpr(length-1)))
            //console.error(last)
            if (last.isNil()) {
                last = new List([])
            }
            if (last instanceof List) {
                for (let i = length - 2; i >= 0; i--) {
                    const value = this.getValue(this.visitQuote(expr.listExpr(i)))
                    last = last.cons(value)
                }
                return last
            }
            const prelast = this.getValue(this.visitQuote(expr.listExpr(length-2)))
            let result = new Pair(prelast, last)
            for (let i = length - 3; i >= 0; i--) {
                const value = this.getValue(this.visitQuote(expr.listExpr(i)))
                result = result.cons(value)
            }
            return result
        } else if (expr instanceof VeLispParser.ListContext) {
            //console.error('LIST:', str);
            const length = expr.listExpr().length
            const values = []
            for (let i = 0; i < length; i++) {
                const value = this.getValue(this.visitQuote(expr.listExpr(i)))
                values.push(value)
            }
            return new List(values)
        } else if (expr instanceof VeLispParser.LambdaContext) {
            return this.visitLambda(expr)
        } else if (expr instanceof VeLispParser.QuoteContext) {
            const name = new Sym(expr.children[1].getText())
            const values = [name]
            for (let i = 2; i < expr.children.length-1; i++) {
                // Fake expr() function returning itself
                expr.children[i].expr = () => expr.children[i]
                const value = this.getValue(this.visitQuote(expr.children[i]))
                values.push(value)
            }
            return new List(values)
        } else if (expr instanceof VeLispParser.AndContext ||
                   expr instanceof VeLispParser.CondContext ||
                   expr instanceof VeLispParser.DefunContext ||
                   expr instanceof VeLispParser.ForeachContext ||
                   expr instanceof VeLispParser.FunctionContext ||
                   expr instanceof VeLispParser.IfContext ||
                   expr instanceof VeLispParser.OrContext ||
                   expr instanceof VeLispParser.PrognContext ||
                   expr instanceof VeLispParser.RepeatContext ||
                   expr instanceof VeLispParser.SetQContext ||
                   expr instanceof VeLispParser.WhileContext) {
            const name = expr.children[1].getText()
            throw new Error(`quote: \`${name}\` not supported`)
        } else {
            //console.error(str);
            //console.error(ctx.expr());
            return new Sym(str)
        }
    }

    visitRepeat(ctx) {
        let result = new Bool(false)
        const count = this.getValue(this.visit(ctx.repeatNum()))
        //console.error('repeat count:', count);

        const line = ctx.start.line
        this.stack.top().callerLine = line

        if (count instanceof Int && count.value() > 0) {
            for (let i = 0; i < count.value(); i++) {
                for (let j = 0; j < ctx.expr().length; j++) {
                    result = this.visit(ctx.expr(j))
                }
            }
        } else {
            throw new Error('repeat: `num` expected positive Int')
        }
        return result
    }

    visitSetQ(ctx) {
        let value = new Bool(false)

        const line = ctx.start.line
        this.stack.top().callerLine = line

        for (let i = 0; i < ctx.setqNameExpr().length; i++) {
            // This argument is not evaluated
            const name = this.visit(ctx.setqNameExpr(i).ID()).toUpperCase()
            value = this.getValue(this.visit(ctx.setqNameExpr(i).expr()))
            //console.error(`setq: ${name} = ${value}`);
            this.stack.top().setVar(name, value)
        }
        return value
    }

    visitWhile(ctx) {
        let result = new Bool(false)

        const line = ctx.start.line
        this.stack.top().callerLine = line

        for (;;) {
            const test = this.getValue(this.visit(ctx.whileTest()))
            //console.error('while test:', test);
            if (!test.isNil()) {
                for (let i = 0; i < ctx.expr().length; i++) {
                    result = this.visit(ctx.expr(i))
                }
            } else {
                break
            }
        }
        return result
    }

    visitTick(ctx) {
        return this.visitQuote(ctx)
    }

    visitDotList(ctx) {
        // Must be call in the quoted context only,
        // otherwise evaluate as a list
        return this.visitList(ctx)
    }

    visitList(ctx) {
        if (ctx.listExpr().length === 0) {
            return new Bool(false)
        }
        const name = ctx.listExpr(0).expr().getText()

        let fun = this.getValue(this.visit(ctx.listExpr(0).expr()))
        // Try to get function out of symbol
        if (!fun.isNil() && fun instanceof Sym) {
            fun = this.stack.top().getSym(fun.value())
        }
        if (fun instanceof Fun) {
            // Evaluate args eagerly
            const args = []
            for (let i = 1; i < ctx.listExpr().length; i++) {
                args.push(this.getValue(this.visit(ctx.listExpr(i).expr())))
            }
            // Create new context
            const topContext = this.stack.top()
            const context = new VeLispContext(topContext)
            context.funName = fun.name
            context.funFile = fun.file
            //context.funArgs = args.map(v => v.toString()) // for debug only
            context.callerFile = (
                !topContext.funFile || topContext.funFile === 'kernel'
                    ? topContext.callerFile
                    : topContext.funFile
            )
            context.callerLine = ctx.start.line

            // Push new context
            this.stack.push(context)

            const trace = isTrace(fun.name)
            let indent

            // Maybe enter trace
            if (trace) {
                indent = ' '.repeat(this.traceDepth * 2)
                const funcall = `(${fun.name} ${args.join(' ')})`
                console.error(`${indent}${this.traceDepth}: Entering ${funcall}`)
                this.traceDepth++
            }

            // Call function
            const result = fun.apply(this, args)

            // Maybe leave trace
            if (trace) {
                this.traceDepth--
                console.error(`${indent}${this.traceDepth}: Result: ${result}`)
            }

            // Pop new context
            this.stack.pop()
            return result
        }

        // Make and push minimal helpful stack context
        // to better locate the error
        const topContext = this.stack.top()
        const context = new VeLispContext(topContext)
        context.funName = name
        context.callerFile = (
            !topContext.funFile || topContext.funFile === 'kernel'
                ? topContext.callerFile
                : topContext.funFile
        )
        context.callerLine = ctx.start.line
        this.stack.push(context)

        throw new Error(`${name}: function not defined`)
    }

    visitTerminal(ctx) {
        const str = ctx.getText()
        if (ctx.parentCtx instanceof VeLispParser.NilContext) {
            //console.error('NIL:', str);
            return new Bool(false)
        } else if (ctx.parentCtx instanceof VeLispParser.TruContext) {
            //console.error('T:', str);
            return new Bool(true)
        } else if (ctx.parentCtx instanceof VeLispParser.IntContext) {
            //console.error('INT:', str);
            return new Int(Number.parseInt(str))
        } else if (ctx.parentCtx instanceof VeLispParser.RealContext) {
            //console.error('REAL:', str);
            return new Real(Number.parseFloat(str))
        } else if (ctx.parentCtx instanceof VeLispParser.StrContext) {
            //console.error('STR:', str);
            // Remove first and last double quotes (")
            return new Str(unescape(str.substring(1, str.length-1)))
        } else if (ctx.parentCtx instanceof VeLispParser.IdContext) {
            //console.error('ID:', str);
            return this.stack.top().getVar(str.toUpperCase())
        } else {
            // Also handles ID outside of expr
            //console.error('TERMINAL:', str);
            //console.error(ctx);
            return str
        }
    }

    makeUFun(name, ctx) {
        const line = ctx.start.line
        this.stack.top().callerLine = line

        const params = []
        const locals = []
        for (let i = 0; i < ctx.funParam().length; i++) {
            const param = this.visit(ctx.funParam(i).ID()).toUpperCase()
            // Workaround (defun | lambda ... (/) ...) case
            if (param !== '/') {
                params.push(param)
            }
        }
        for (let i = 0; i < ctx.funLocal().length; i++) {
            const local = this.visit(ctx.funLocal(i).ID()).toUpperCase()
            locals.push(local)
        }
        return new UFun(
            name,
            params,
            locals,
            (self, args) => {
                if (args.length < params.length) {
                    throw new Error(`${name}: too few arguments`)
                } else if (args.length > params.length) {
                    throw new Error(`${name}: too many arguments`)
                }
                // Since locals with the same names as params will reset the values, init locals first.
                const context = self.stack.top()
                for (let i = 0; i < locals.length; i++) {
                    context.initVar(locals[i], new Bool(false))
                }
                for (let i = 0; i < params.length; i++) {
                    context.initVar(params[i], args[i])
                }
                let result = new Bool(false)
                for (let i = 0; i < ctx.expr().length; i++) {
                    result = this.getValue(self.visit(ctx.expr(i)))
                }
                return result
            },
            this.stack.top().callerFile
        )
    }

    getValue(expr) {
        if (expr instanceof Array) {
            return this.getValue(expr[0])
        }
        return expr
    }
}

export default VeLispEvalVisitor

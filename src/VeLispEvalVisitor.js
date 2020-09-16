const {VeLispParser} = require('../grammar/VeLispParser.js');
const {VeLispVisitor} = require('../grammar/VeLispVisitor.js');
const {VeLispContext} = require('./VeLispContext.js');
const {Bool, Int, Real, Str, Sym, List, Fun} = require('./VeLispTypes.js');

class EvalVisitor extends VeLispVisitor {
    constructor(context) {
        super();
        this.contexts = [context];
    }

    // Special Forms (AutoCAD 2013 AutoLISP Developer's Guild p.37)

    visitAnd(ctx) {
        for (let i = 0; i < ctx.expr().length; i++) {
            const result = this.getValue(this.visit(ctx.expr(i)));
            if (result.isNil()) {
                return new Bool(false);
            }
        }
        return new Bool(true);
    }

    visitCond(ctx) {
        let result = new Bool(false);
        for (let i = 0; i < ctx.condTestResult().length; i++) {
            const test = this.getValue(this.visit(ctx.condTestResult(i).condTest()));
            //console.error('cond test:', test);
            if (!test.isNil()) {
                result = this.visit(ctx.condTestResult(i).condResult());
                break;
            }
        }
        return result;
    }

    visitDefun(ctx) {
        let name = this.visit(ctx.defunName().ID()).toUpperCase();
        //console.error(`(defun ${name} ...)`);
        let params = [];
        let locals = [];
        for (let i = 0; i < ctx.defunParam().length; i++) {
            params.push(this.visit(ctx.defunParam(i).ID()).toUpperCase());
        }
        for (let i = 0; i < ctx.defunLocal().length; i++) {
            locals.push(this.visit(ctx.defunLocal(i).ID()).toUpperCase());
        }
        // TODO: Are params really needed here?
        this.contexts[this.contexts.length-1].setSym(name, new Fun(name, params, function (self, args) {
            if (args.length < params.length) {
                throw new Error(`${name}: too few arguments`);
            } else if (args.length > params.length) {
                throw new Error(`${name}: too many arguments`);
            }
            // Since locals with the same names as params will reset the values, init locals first.
            for (let i = 0; i < locals.length; i++) {
                self.contexts[self.contexts.length-1].initVar(locals[i], new Bool(false));
            }
            for (let i = 0; i < params.length; i++) {
                self.contexts[self.contexts.length-1].initVar(params[i], args[i]);
            }
            let result = new Bool(false);
            for (let i = 0; i < ctx.expr().length; i++) {
                result = self.visit(ctx.expr(i));
            }
            return result;
        }));
        return new Sym(name);
    }

    visitForeach(ctx) {
        let name = this.visit(ctx.foreachName().ID()).toUpperCase();
        const list = this.getValue(this.visit(ctx.foreachList()));
        //console.error(`foreach: ${name} ${list}`);
        if (list instanceof List) {
            let result = new Bool(false);
            // Push new context with 'name' = nil
            const context = new VeLispContext(this.contexts[this.contexts.length-1]);
            context.initVar(name, new Bool(false));
            this.contexts.push(context);
            for (let i = 0; i < list.value().length; i++) {
                // Set each list value directly into our context, not the latest context,
                // to make it possible to shadow 'name' in some down context
                const value = list.value()[i];
                //console.error(`foreach: ${value}`);
                context.setVar(name, value);
                for (let j = 0; j < ctx.expr().length; j++) {
                    result = this.visit(ctx.expr(j));
                }
            }
            // Pop new context
            this.contexts.pop();
            return result;
        }
        throw new Error('foreach: list must be List');
    }

    visitIf(ctx) {
        const test = this.getValue(this.visit(ctx.ifTest()));
        //console.error('if test:', test);
        if (!test.isNil()) {
            return this.visit(ctx.ifThen());
        } else {
            if (ctx.ifElse()) {
                return this.visit(ctx.ifElse());
            } else {
                return new Bool(false);
            }
        }
    }

    visitOr(ctx) {
        for (let i = 0; i < ctx.expr().length; i++) {
            const result = this.getValue(this.visit(ctx.expr(i)));
            if (!result.isNil()) {
                return new Bool(true);
            }
        }
        return new Bool(false);
    }

    visitProgn(ctx) {
        let result = new Bool(false);
        for (let i = 0; i < ctx.expr().length; i++) {
            result = this.visit(ctx.expr(i));
        }
        return result;
    }

    visitRepeat(ctx) {
        let result = new Bool(false);
        let count = this.getValue(this.visit(ctx.repeatNum()));
        //console.error('repeat count:', count);
        if (count instanceof Int && count.value() > 0) {
            for (let i = 0; i < count.value(); i++) {
                for (let j = 0; j < ctx.expr().length; j++) {
                    result = this.visit(ctx.expr(j));
                }
            }
        } else {
            throw new Error(`repeat: num expected to be positive integer, but saw ${count}`);
        }
        return result;
    }

    visitSetQ(ctx) {
        let value = new Bool(false);
        for (let i = 0; i < ctx.setqNameExpr().length; i++) {
            // This argument is not evaluated
            const name = this.visit(ctx.setqNameExpr(i).ID()).toUpperCase();
            value = this.visit(ctx.setqNameExpr(i).expr());
            //console.error(`setq: ${name} = ${value}`);
            this.contexts[this.contexts.length-1].setVar(name, value);
        }
        return value;
    }

    visitWhile(ctx) {
        let result = new Bool(false);
        while (true) {
            const test = this.getValue(this.visit(ctx.whileTest()));
            //console.error('while test:', test);
            if (!test.isNil()) {
                for (let i = 0; i < ctx.expr().length; i++) {
                    result = this.visit(ctx.expr(i));
                }
            } else {
                break;
            }
        }
        return result;
    }

    visitPrinc(ctx) {
        let expr = this.getValue(this.visit(ctx.expr()));
        console.log(expr.toString());
        return expr;
    }

    visitFun(ctx) {
        let name = this.visit(ctx.ID()).toUpperCase();
        //console.log(`funName: ${name}`);
        let fun = null;
        // Try to get symbol out of variable
        const local = this.contexts[this.contexts.length-1].getVar(name);
        // If there's such a variable and it's a symbol, try to find the function
        if (!local.isNil() && local instanceof Sym) {
            fun = this.contexts[this.contexts.length-1].getSym(local.value());
        }
        // If the above failed, try to find the function directly by name
        if (!fun || fun.isNil()) {
            fun = this.contexts[this.contexts.length-1].getSym(name);
        }
        if (fun instanceof Fun) {
            // Evaluate args eagerly
            let args = [];
            for (let i = 0; i < ctx.funArg().length; i++) {
                args.push(this.getValue(this.visit(ctx.funArg(i))));
            }
            //console.error(`(${name} ${args.join(' ')})`);
            // Push new context
            this.contexts.push(new VeLispContext(this.contexts[this.contexts.length-1]));
            const result = fun.apply(this, args);
            // Pop new context
            this.contexts.pop();
            return result;
        }
        throw new Error(`unknown function ${name}`);
    }

    visitTerminal(ctx) {
        const str = ctx.getText();
        if (ctx.parentCtx instanceof VeLispParser.NilContext) {
            //console.error('NIL:', str);
            return new Bool(false);
        } else if (ctx.parentCtx instanceof VeLispParser.TruContext) {
            //console.error('T:', str);
            return new Bool(true);
        } else if (ctx.parentCtx instanceof VeLispParser.IntContext) {
            //console.error('INT:', str);
            return new Int(Number.parseInt(str));
        } else if (ctx.parentCtx instanceof VeLispParser.RealContext) {
            //console.error('REAL:', str);
            return new Real(Number.parseFloat(str));
        } else if (ctx.parentCtx instanceof VeLispParser.StrContext) {
            //console.error('STR:', str);
            return new Str(str.replace(/\"/g, ''));
        } else if (ctx.parentCtx instanceof VeLispParser.SymContext) {
            //console.error('SYM:', str);
            return new Sym(str.replace(/\'/g, ''));
        } else if (ctx.parentCtx instanceof VeLispParser.IdContext) {
            //console.error('ID:', str);
            return this.contexts[this.contexts.length-1].getVar(str.toUpperCase());
        } else {
            // Also handles ID outside of expr
            //console.error('TERMINAL:', str);
            //console.error(ctx);
            return str;
        }
    }

    getValue(expr) {
        if (expr instanceof Array) {
            return this.getValue(expr[0]);
        }
        return expr;
    }
}

exports.EvalVisitor = EvalVisitor;

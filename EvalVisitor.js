import {AutoLISPParser} from './grammar/AutoLISPParser.js';
import {AutoLISPVisitor} from './grammar/AutoLISPVisitor.js';
import {AutoLISPContext} from './AutoLISPContext.js';
import {Bool, Int, Real, Str, List, Fun} from './AutoLISPTypes.js';

export class EvalVisitor extends AutoLISPVisitor {
    constructor(context) {
        super();
        this.contexts = [context];
    }

    visitCond(ctx) {
        let result = new Bool(false);
        for (let i = 0; i < ctx.testresult().length; i++) {
            const test = this.getValue(this.visit(ctx.testresult(i).testexpr()));
            //console.error('cond test:', test);
            if (test.isTruthy()) {
                result = this.visit(ctx.testresult(i).resultexpr());
                break;
            }
        }
        return result;
    }

    visitDefun(ctx) {
        let name = this.visit(ctx.ID(0));
        //console.error(`(defun ${name} ...)`);
        let params = [];
        for (let i = 1; i < ctx.ID().length; i++) {
            params.push(this.visit(ctx.ID(i)));
        }
        this.contexts[this.contexts.length-1].setSym(name, new Fun(name, params, function (self, args) {
            if (args.length < params.length) {
                throw new Error(`${name}: too few arguments`);
            } else if (args.length > params.length) {
                throw new Error(`${name}: too many arguments`);
            }
            for (let i = 0; i < params.length; i++) {
                self.contexts[self.contexts.length-1].setVar(params[i], args[i]);
            }
            let result = new Bool(false);
            for (let i = 0; i < ctx.expr().length; i++) {
                result = self.visit(ctx.expr(i));
            }
            return result;
        }));
        return name; // TODO: new Sym(name)?
    }

    visitIf(ctx) {
        const test = this.getValue(this.visit(ctx.testexpr()));
        //console.error('if test:', test);
        if (test.isTruthy()) {
            return this.visit(ctx.thenexpr());
        } else {
            if (ctx.elseexpr()) {
                return this.visit(ctx.elseexpr());
            } else {
                return new Bool(false);
            }
        }
    }

    visitRepeat(ctx) {
        let result = new Bool(false);
        let count = this.getValue(this.visit(ctx.numexpr()));
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
        // TODO: no args
        let value;
        for (let i = 0; i < ctx.idexpr().length; i++) {
            const id = this.getValue(this.visit(ctx.idexpr(i).ID()));
            value = this.getValue(this.visit(ctx.idexpr(i).expr()));
            //console.error(`setq ${id} = ${val}`);
            this.contexts[this.contexts.length-1].setVar(id, value);
        }
        return value;
    }

    visitWhile(ctx) {
        let result = new Bool(false);
        while (true) {
            const test = this.getValue(this.visit(ctx.testexpr()));
            //console.error('while test:', test);
            if (test.isTruthy()) {
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

    visitFunCall(ctx) {
        //let id = this.getValue(this.visit(ctx.funexpr()));
        let name = this.visit(ctx.ID());
        const fun = this.contexts[this.contexts.length-1].getSym(name);
        if (fun instanceof Fun) {
            // Evaluate args eagerly
            let args = [];
            for (let i = 0; i < ctx.argexpr().length; i++) {
                args.push(this.getValue(this.visit(ctx.argexpr(i))));
            }
            //console.error(`(${name} ${args.join(' ')})`);
            this.contexts.push(new AutoLISPContext(this.contexts[this.contexts.length-1]));
            const result = fun.apply(this, args);
            this.contexts.pop();
            return result;
        }
        throw new Error(`unknown function ${name}`);
    }

    visitTerminal(ctx) {
        const str = ctx.getText();
        if (ctx.parentCtx instanceof AutoLISPParser.NilContext) {
            //console.error('NIL:', str);
            return new Bool(false);
        } else if (ctx.parentCtx instanceof AutoLISPParser.TContext) {
            //console.error('T:', str);
            return new Bool(true);
        } else if (ctx.parentCtx instanceof AutoLISPParser.IntContext) {
            //console.error('INT:', str);
            return new Int(Number.parseInt(str));
        } else if (ctx.parentCtx instanceof AutoLISPParser.RealContext) {
            //console.error('REAL:', str);
            return new Real(Number.parseFloat(str));
        } else if (ctx.parentCtx instanceof AutoLISPParser.StrContext) {
            //console.error('STR:', str);
            return new Str(str.replace(/\"/g, ''));
        } else if (ctx.parentCtx instanceof AutoLISPParser.IdContext) {
            //console.error('ID:', str);
            return this.contexts[this.contexts.length-1].getVar(str);
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

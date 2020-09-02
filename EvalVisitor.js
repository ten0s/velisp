import {AutoLISPParser} from './grammar/AutoLISPParser';
import {AutoLISPVisitor} from './grammar/AutoLISPVisitor';
import {Integer, Real, String, List} from './AutoLISPTypes';

export class EvalVisitor extends AutoLISPVisitor {
    constructor() {
        super();
        this.vars = {};
    }

    visitMultiply(ctx) {
        if (ctx.expr().length == 0) {
            return new Integer(0);
        }
        let result = new Integer(1);
        //console.error('multiply:', result);
        for (let i = 0; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            //console.error('multiply:', arg);
            result = result.multiply(arg);
        }
        return result;
    }

    visitDivide(ctx) {
        if (ctx.expr().length == 0) {
            return new Integer(0);
        }
        let result = this.getValue(this.visit(ctx.expr(0)));
        //console.error('divide:', result);
        for (let i = 1; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            //console.error('divide:', arg);
            result = result.divide(arg);
        }
        return result;
    }

    visitAdd(ctx) {
        let result = new Integer(0);
        //console.error('add:', result);
        for (let i = 0; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            //console.error('add:', arg);
            result = result.add(arg);
        }
        return result;
    }
    
    visitSubtract(ctx) {
        if (ctx.expr().length == 0) {
            return new Integer(0);
        }
        let result = this.getValue(this.visit(ctx.expr(0)));
        //console.error('subtract:', result);
        if (ctx.expr().length == 1) {
            return result.multiply(new Integer(-1));
        }
        for (let i = 1; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            //console.error('subtract:', arg);
            result = result.subtract(arg);
        }
        return result;
    }

    visitEqualTo(ctx) {
        let result = true;
        let val1 = this.getValue(this.visit(ctx.expr(0)));
        console.error('equalTo:', val1);
        for (let i = 1; i < ctx.expr().length; i++) {
            const val2 = this.getValue(this.visit(ctx.expr(i)));
            console.error('equalTo:', val2);
            result = (val1 == val2);
            if (!result) break;
            val1 = val2;
        }
        return result;
    }

    visitGreaterThan(ctx) {
        let result = true;
        let val1 = this.getValue(this.visit(ctx.expr(0)));
        for (let i = 1; i < ctx.expr().length; i++) {
            const val2 = this.getValue(this.visit(ctx.expr(i)));
            result = (val1 > val2);
            if (!result) break;
            val1 = val2;
        }
        return result;
    }

    visitList(ctx) {
        let result = [];
        for (let i = 0; i < ctx.expr().length; i++) {
            const val = this.getValue(this.visit(ctx.expr(i)));
            result.push(val);
        }
        return new List(result);
    }

    visitCar(ctx) {
        let list = this.getValue(this.visit(ctx.expr()));
        return list.car();
    }

    visitCdr(ctx) {
        let list = this.getValue(this.visit(ctx.expr()));
        return list.cdr();
    }

    visitSetQ(ctx) {
        const key = this.getValue(this.visit(ctx.ID()));
        const val = this.getValue(this.visit(ctx.expr()));
        console.error('setq:', key, val);
        this.vars[key] = val;
        return val;
    }

    visitIf(ctx) {
        const test = this.getValue(this.visit(ctx.testexpr()));
        console.error('if test:', test);
        if (test) {
            return this.visit(ctx.thenexpr());
        } else {
            return this.visit(ctx.elseexpr());
        }
    }

    visitWhile(ctx) {
        let ret = null;
        while (true) {
            const test = this.getValue(this.visit(ctx.testexpr()));
            console.error('while test:', test);
            if (test) {
                for (let i = 0; i < ctx.expr().length; i++) {
                    ret = this.visit(ctx.expr(i));
                }
            } else {
                break;
            }
        }
        return ret;
    }

    visitPrinc(ctx) {
        let expr = this.getValue(this.visit(ctx.expr()));
        console.log('princ:', expr.toString());
        return expr;
    }

    visitTerminal(ctx) {
        const str = ctx.getText();
        if (ctx.parentCtx instanceof AutoLISPParser.IntegerContext) {
            //console.error('INTEGER:', str);
            return new Integer(Number.parseInt(str));
        } else if (ctx.parentCtx instanceof AutoLISPParser.RealContext) {
            //console.error('REAL:', str);
            return new Real(Number.parseFloat(str));
        } else if (ctx.parentCtx instanceof AutoLISPParser.StringContext) {
            //console.error('STRING:', str);
            return new String(str.replace(/\"/g, ''));
        } else if (ctx.parentCtx instanceof AutoLISPParser.VariableContext) {
            //console.error('VARIABLE:', str);
            return this.vars[str];
        } else {
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

    /*
    visitChildren(ctx) {
        if (!ctx) {
            return;
        }
        
        if (ctx.children) {
            return ctx.children.map(child => {
                if (child.children && child.children.length != 0) {
                    return child.accept(this);
                } else {
                    return child.getText();
                }
            });
        }
    }
    */

    /*
    visitChildren(ctx) {
        let code = '';
        
        for (let i = 0; i < ctx.getChildCount(); i++) {
            code += this.visit(ctx.getChild(i));
        }
        
        return code.trim();
    }
    */
}

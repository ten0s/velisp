const AutoLispParser = require('./grammar/AutoLispParser').AutoLispParser;
const AutoLispVisitor = require('./grammar/AutoLispVisitor').AutoLispVisitor;

class EvalVisitor extends AutoLispVisitor {
    constructor() {
        super();
        this.vars = {};
    }

    visitMultiply(ctx) {
        let result = 1;
        for (let i = 0; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            console.error("multiply:", arg);
            result *= arg;
        }
        return result;
    }

    visitDivide(ctx) {
        let result = this.getValue(this.visit(ctx.expr(0)));
        for (let i = 1; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            console.error("divide:", arg);
            result /= arg;
        }
        return result;
    }

    visitAdd(ctx) {
        let result = 0;
        for (let i = 0; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            console.error("add:", arg);
            result += arg;
        }
        return result;
    }
    
    visitSubtract(ctx) {
        let result = this.getValue(this.visit(ctx.expr(0)));
        console.error("subtract:", result);
        if (ctx.expr().length == 1) {
            return -result;
        }
        for (let i = 1; i < ctx.expr().length; i++) {
            const arg = this.getValue(this.visit(ctx.expr(i)));
            console.error("subtract:", arg);
            result -= arg;
        }
        return result;
    }

    visitEqualTo(ctx) {
        let result = true;
        let val1 = this.getValue(this.visit(ctx.expr(0)));
        console.error("equalTo:", val1);
        for (let i = 1; i < ctx.expr().length; i++) {
            const val2 = this.getValue(this.visit(ctx.expr(i)));
            console.error("equalTo:", val2);
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
        return result;
    }

    visitSetQ(ctx) {
        const key = this.getValue(this.visit(ctx.ID()));
        const val = this.getValue(this.visit(ctx.expr()));
        console.error("setq:", key, val);
        this.vars[key] = val;
        return val;
    }

    visitIf(ctx) {
        const test = this.getValue(this.visit(ctx.testexpr()));
        console.error("if test:", test);
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
            console.error("while test:", test);
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
        console.log("princ:", expr);
        return expr;
    }

    visitTerminal(ctx) {
        if (ctx.parentCtx instanceof AutoLispParser.IdContext) {
            const id = ctx.getText();
            console.error("ID:", id);
            return this.vars[id];
        } else if (ctx.parentCtx instanceof AutoLispParser.IntegerContext) {
            console.error("INTEGER:", ctx.getText());
            return Number.parseInt(ctx.getText());
        } else if (ctx.parentCtx instanceof AutoLispParser.RealContext) {
            console.error("REAL:", ctx.getText());
            // TODO: still returns INT if 2.0,
            // TODO: need some hierarchy of Integer, Real, Boolean, etc to be implemented
            return Number.parseFloat(ctx.getText()) * 1.0;
        } else if (ctx.parentCtx instanceof AutoLispParser.StringContext) {
            console.error("STRING:", ctx.getText());
            return ctx.getText();
        } else {
            console.error("TERMINAL:", ctx.getText());
            //console.error(ctx);
            return ctx.getText();
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

module.exports = EvalVisitor;

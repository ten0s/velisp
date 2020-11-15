const fs = require('fs');
const antlr4 = require('antlr4');
const {VeDclLexer} = require('../grammar/VeDclLexer.js');
const {VeDclParser} = require('../grammar/VeDclParser.js');
const {VeDclListener} = require('../grammar/VeDclListener.js');
const {Dialog, Row, Column, Text, Button, EditBox} = require('./VeDclControls.js');

class VeDclDialogsLoader extends VeDclListener {
    constructor(context) {
        super();
        this.dialogs = [];
        this.clusters = [];
        this.controls = [];
    }

    enterFile(ctx) {
    };

    exitFile(ctx) {
    };

    enterDialog(ctx) {
        console.log('enterDialog');
        const id = ctx.ID().getText();
        const dialog = new Dialog(id);
        this.dialogs.push(dialog);
        this.clusters.push(dialog);
        this.controls.push(dialog); // why?
    };

    exitDialog(ctx) {
        console.log('exitDialog');
        this.clusters.pop();
        this.controls.pop();
    };

    enterRow(ctx) {
        console.log('enterRow');
        const row = new Row();
        this.clusters.push(row);
        this.controls.push(row);
    }

    exitRow(ctx) {
        console.log('exitRow');
        const cluster = this.clusters.pop();
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    }

    enterColumn(ctx) {
        console.log('enterColumn');
        const col = new Column();
        this.clusters.push(col);
        this.controls.push(col);
    }

    exitColumn(ctx) {
        console.log('exitColumn');
        const cluster = this.clusters.pop();
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    }

    enterText(ctx) {
        console.log('enterText');
        const id = ctx.ID() ? ctx.ID().getText() : '';
        const text = new Text(id);
        this.controls.push(text);
    };

    exitText(ctx) {
        console.log('exitText');
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    };

    enterEditBox(ctx) {
        console.log('enterEditBox');
        const id = ctx.ID() ? ctx.ID().getText() : '';
        const editbox = new EditBox(id);
        this.controls.push(editbox);
    };

    exitEditBox(ctx) {
        console.log('exitEditBox');
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    };

    enterButton(ctx) {
        console.log('enterButton');
        const id = ctx.ID() ? ctx.ID().getText() : '';
        const button = new Button(id);
        this.controls.push(button);
    };

    exitButton(ctx) {
        console.log('exitButton');
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    };

    enterControl(ctx) {
        //this.controls.push(null);
    };

    exitControl(ctx) {
        //this.controls.pop();
    };

    enterAttribute(ctx) {
        const current = this.controls[this.controls.length-1];
        if (current) {
            const name = ctx.attributeName().ID().getText();
            let value = null;
            if (ctx.attributeValue().BOOL()) {
                value = ctx.attributeValue().BOOL().getText() === 'true';
            } else if (ctx.attributeValue().INT()) {
                value = Number.parseInt(ctx.attributeValue().INT().getText());
            } else if (ctx.attributeValue().REAL()) {
                value = Number.parseFloat(ctx.attributeValue().REAL().getText());
            } else if (ctx.attributeValue().STR()) {
                // Remove first and last double quotes (")
                const str = ctx.attributeValue().STR().getText();
                value = str.substring(1, str.length-1);
            } else if (ctx.attributeValue().ALIGN()) {
                value = ctx.attributeValue().ALIGN().getText();
            } else {
                throw new Error(`Unhandled: ${name} = ${ctx.attributeValue().getText()}`);
            }
            current.addAttribute(name, value);
        }
    };

    exitAttribute(ctx) {
    };

    /*
    visitTerminal(ctx) {
        const str = ctx.getText();
        // Also handles ID outside of expr
        //console.error('TERMINAL:', str);
        //console.error(ctx);
    }
    */

    getValue(expr) {
        if (expr instanceof Array) {
            return this.getValue(expr[0]);
        }
        return expr;
    }
}

function load(dclfile) {
    const dcl = readFile(dclfile);
    const {tree} = parseDcl(dcl);
    const loader = new VeDclDialogsLoader();
    const walker = new antlr4.tree.ParseTreeWalker();
    walker.walk(loader, tree);
    return loader.dialogs;
}

function tree(dclfile) {
    const dcl = readFile(dclfile);
    const {parser, tree} = parseDcl(dcl);
    return tree.toStringTree(parser.ruleNames);
}

function readFile(file) {
    // TODO: handle errors
    return fs.readFileSync(file).toString();
}

function parseDcl(dcl) {
    const chars = new antlr4.InputStream(dcl);
    const lexer = new VeDclLexer(chars);
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new VeDclParser(tokens);
    //parser.removeErrorListeners();
    //parser.addErrorListener(new VeDclErrorListener());
    return {
        lexer,
        tokens,
        parser,
        tree: parser.file(),
    };
}

exports.load = load;
exports.tree = tree;

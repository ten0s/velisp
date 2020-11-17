const fs = require('fs');
const antlr4 = require('antlr4');
const {VeDclLexer} = require('../grammar/VeDclLexer.js');
const {VeDclParser} = require('../grammar/VeDclParser.js');
const {VeDclListener} = require('../grammar/VeDclListener.js');
const {Dialog, Row, Column, Text, Button, EditBox} = require('./VeDclControls.js');

const tileCtors = {
    'dialog'  : (id) => new Dialog(id),
    'row'     : () => new Row(),
    'column'  : () => new Column(),
    'text'    : () => new Text(),
    'button'  : () => new Button(),
    'edit_box': () => new EditBox(),
};

class VeDclDialogsLoader extends VeDclListener {
    constructor(context) {
        super();
        this.defines = {};
        this.clusters = [];
        this.controls = [];
    }

    get dialogs() {
        const dialogs = [];
        for (const obj of Object.values(this.defines)) {
            if (obj instanceof Dialog) {
                dialogs.push(obj);
            }
        }
        return dialogs;
    }

    enterFile(ctx) {
    };

    exitFile(ctx) {
    };

    enterDefineClusterTile(ctx) {
        console.log('enterDefineClusterTile');
        const tileId = ctx.ID().getText();
        const tileName = ctx.clusterTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor(tileId);
        this.defines[tileId] = tile;
        this.clusters.push(tile);
        this.controls.push(tile);
    };

    exitDefineClusterTile(ctx) {
        console.log('exitDefineClusterTile');
        this.clusters.pop();
        this.controls.pop();
    };

    enterDefineSimpleTile(ctx) {
        console.log('enterDefineSimpleTile');
        const tileId = ctx.ID().getText();
        const tileName = ctx.simpleTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor(tileId);
        this.defines[tileId] = tile;
        this.controls.push(tile);
    };

    exitDefineSimpleTile(ctx) {
        console.log('exitDefineSimpleTile');
        this.controls.pop();
    };

    enterInnerClusterTile(ctx) {
        console.log('enterInnerClusterTile');
        const tileName = ctx.clusterTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor();
        this.clusters.push(tile);
        this.controls.push(tile);
    }

    exitInnerClusterTile(ctx) {
        console.log('exitInnerClusterTile');
        const cluster = this.clusters.pop();
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    }

    enterInnerSimpleTile(ctx) {
        console.log('enterInnerSimpleTile');
        const tileName = ctx.simpleTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor();
        this.controls.push(tile);
    }

    exitInnerSimpleTile(ctx) {
        console.log('exitInnerSimpleTile');
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    }

    enterInnerDeriveTile(ctx) {
        console.log('enterInnerDeriveTile');
        const tileName = ctx.deriveTile().ID().getText();
        const tile = this.defines[tileName];
        // TODO: clone tile
        // TODO: tile == null
        this.controls.push(tile);
    }

    exitInnerDeriveTile(ctx) {
        console.log('exitInnerDeriveTile');
        this.clusters[this.clusters.length-1].addControl(this.controls.pop());
    }

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

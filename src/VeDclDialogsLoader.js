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
        this.context = context;
    }

    get dialogs() {
        return Object.values(this.context.defines).filter(obj => obj instanceof Dialog);
    }

    enterFile(ctx) {
    };

    exitFile(ctx) {
    };

    enterIncludeFile(ctx) {
        console.log('enterIncludeFile');
        // Remove first and last double quotes (")
        const str = ctx.fileName().STR().getText();
        const filename = str.substring(1, str.length-1);
        load(filename, this.context);
    }

    exitIncludeFile(ctx) {
        console.log('exitIncludeFile');
        console.log(this.context);
    }

    enterDefineClusterTile(ctx) {
        console.log('enterDefineClusterTile');
        const tileId = ctx.ID().getText();
        const tileName = ctx.clusterTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor(tileId);
        this.context.defines[tileId] = tile;
        this.context.clusters.push(tile);
        this.context.controls.push(tile);
    };

    exitDefineClusterTile(ctx) {
        console.log('exitDefineClusterTile');
        this.context.clusters.pop();
        this.context.controls.pop();
    };

    enterDefineSimpleTile(ctx) {
        console.log('enterDefineSimpleTile');
        const tileId = ctx.ID().getText();
        const tileName = ctx.simpleTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor(tileId);
        this.context.defines[tileId] = tile;
        this.context.controls.push(tile);
    };

    exitDefineSimpleTile(ctx) {
        console.log('exitDefineSimpleTile');
        this.context.controls.pop();
    };

    enterInnerClusterTile(ctx) {
        console.log('enterInnerClusterTile');
        const tileName = ctx.clusterTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor();
        this.context.clusters.push(tile);
        this.context.controls.push(tile);
    }

    exitInnerClusterTile(ctx) {
        console.log('exitInnerClusterTile');
        const cluster = this.context.clusters.pop();
        this.context.clusters[this.context.clusters.length-1].addControl(this.context.controls.pop());
    }

    enterInnerSimpleTile(ctx) {
        console.log('enterInnerSimpleTile');
        const tileName = ctx.simpleTile().getText();
        const tileCtor = tileCtors[tileName];
        // TODO: tileCtor == null
        const tile = tileCtor();
        this.context.controls.push(tile);
    }

    exitInnerSimpleTile(ctx) {
        console.log('exitInnerSimpleTile');
        this.context.clusters[this.context.clusters.length-1].addControl(this.context.controls.pop());
    }

    enterInnerDeriveTile(ctx) {
        console.log('enterInnerDeriveTile');
        const tileName = ctx.deriveTile().ID().getText();
        const tile = this.context.defines[tileName];
        // TODO: tile == null
        // TODO: clone tile
        this.context.controls.push(tile);
    }

    exitInnerDeriveTile(ctx) {
        console.log('exitInnerDeriveTile');
        this.context.clusters[this.context.clusters.length-1].addControl(this.context.controls.pop());
    }

    enterInnerAliasTile(ctx) {
        console.log('enterInnerAliasTile');
        const tileName = ctx.aliasTile().ID().getText();
        const tile = this.context.defines[tileName];
        // TODO: tile == null
        this.context.controls.push(tile);
    }

    exitInnerAliasTile(ctx) {
        console.log('exitInnerAliasTile');
        this.context.clusters[this.context.clusters.length-1].addControl(this.context.controls.pop());
    }

    enterAttribute(ctx) {
        const current = this.context.controls[this.context.controls.length-1];
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

function load(dclfile, context) {
    const dcl = readFile(dclfile);
    const {tree} = parseDcl(dcl);
    const loader = new VeDclDialogsLoader(context);
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

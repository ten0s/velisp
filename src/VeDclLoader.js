const fs = require('fs');
const antlr4 = require('antlr4');
const {VeDclLexer} = require('../grammar/VeDclLexer.js');
const {VeDclParser} = require('../grammar/VeDclParser.js');
const {VeDclListener} = require('../grammar/VeDclListener.js');
const {
    // Clusters
    Dialog,
    Row,
    Column,
    Concatenation,
    RadioRow,
    RadioColumn,
    // Tiles
    Button,
    EditBox,
    RadioButton,
    Spacer,
    Text,
    Toggle,
} = require('./VeDclTiles.js');

const tileCtors = {
    // Clusters
    'dialog'       : (id) => new Dialog(id),
    'row'          : (id) => new Row(id),
    'column'       : (id) => new Column(id),
    'concatenation': (id) => new Concatenation(id),
    'radio_row'    : (id) => new RadioRow(id),
    'radio_column' : (id) => new RadioColumn(id),
    // Tiles
    'button'       : (id) => new Button(id),
    'edit_box'     : (id) => new EditBox(id),
    'radio_button' : (id) => new RadioButton(id),
    'spacer'       : (id) => new Spacer(id),
    'text'         : (id) => new Text(id),
    'toggle'       : (id) => new Toggle(id),
};

class VeDclLoader extends VeDclListener {
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
        //console.log('enterIncludeFile');
        // Remove first and last double quotes (")
        const str = ctx.fileName().STR().getText();
        const filename = str.substring(1, str.length-1);
        load(filename, this.context);
    }

    exitIncludeFile(ctx) {
        //console.log('exitIncludeFile');
        console.log(this.context);
    }

    enterDefineClusterTile(ctx) {
        //console.log('enterDefineClusterTile');
        const tileId = ctx.ID().getText();
        const tileName = ctx.clusterTile().getText();
        const tileCtor = tileCtors[tileName];
        if (!tileCtor) {
            throw new Error(`Unknown tile constructor: ${tileName}`);
        }
        const tile = tileCtor(tileId);
        this.context.defines[tileId] = tile;
        this.context.clusters.push(tile);
        this.context.tiles.push(tile);
    };

    exitDefineClusterTile(ctx) {
        //console.log('exitDefineClusterTile');
        this.context.clusters.pop();
        this.context.tiles.pop();
    };

    enterDefineSimpleTile(ctx) {
        //console.log('enterDefineSimpleTile');
        const tileId = ctx.ID().getText();
        const tileName = ctx.simpleTile().getText();
        const tileCtor = tileCtors[tileName];
        if (!tileCtor) {
            throw new Error(`Unknown tile constructor: ${tileName}`);
        }
        const tile = tileCtor(tileId);
        this.context.defines[tileId] = tile;
        this.context.tiles.push(tile);
    };

    exitDefineSimpleTile(ctx) {
        //console.log('exitDefineSimpleTile');
        this.context.tiles.pop();
    };

    enterInnerClusterTile(ctx) {
        //console.log('enterInnerClusterTile');
        const tileName = ctx.clusterTile().getText();
        const tileCtor = tileCtors[tileName];
        if (!tileCtor) {
            throw new Error(`Unknown tile constructor: ${tileName}`);
        }
        const tile = tileCtor();
        this.context.clusters.push(tile);
        this.context.tiles.push(tile);
    }

    exitInnerClusterTile(ctx) {
        //console.log('exitInnerClusterTile');
        const cluster = this.context.clusters.pop();
        this.context.clusters[this.context.clusters.length-1].addTile(this.context.tiles.pop());
    }

    enterInnerSimpleTile(ctx) {
        //console.log('enterInnerSimpleTile');
        const tileName = ctx.simpleTile().getText();
        const tileCtor = tileCtors[tileName];
        if (!tileCtor) {
            throw new Error(`Unknown tile constructor: ${tileName}`);
        }
        const tile = tileCtor();
        this.context.tiles.push(tile);
    }

    exitInnerSimpleTile(ctx) {
        //console.log('exitInnerSimpleTile');
        this.context.clusters[this.context.clusters.length-1].addTile(this.context.tiles.pop());
    }

    enterInnerDeriveTile(ctx) {
        //console.log('enterInnerDeriveTile');
        const tileName = ctx.deriveTile().ID().getText();
        const tile = this.context.defines[tileName];
        if (!tile) {
            throw new Error(`Unknown tile: ${tileName}`);
        }
        const clone = tile.clone();
        this.context.tiles.push(clone);
    }

    exitInnerDeriveTile(ctx) {
        //console.log('exitInnerDeriveTile');
        this.context.clusters[this.context.clusters.length-1].addTile(this.context.tiles.pop());
    }

    enterInnerAliasTile(ctx) {
        //console.log('enterInnerAliasTile');
        const tileName = ctx.aliasTile().ID().getText();
        const tile = this.context.defines[tileName];
        if (!tile) {
            throw new Error(`Unknown tile: ${tileName}`);
        }
        this.context.tiles.push(tile);
    }

    exitInnerAliasTile(ctx) {
        //console.log('exitInnerAliasTile');
        this.context.clusters[this.context.clusters.length-1].addTile(this.context.tiles.pop());
    }

    enterAttribute(ctx) {
        const current = this.context.tiles[this.context.tiles.length-1];
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
}

function load(dclfile, context) {
    const dcl = readFile(dclfile);
    const {tree} = parseDcl(dcl);
    const loader = new VeDclLoader(context);
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

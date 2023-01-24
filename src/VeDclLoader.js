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

import fs from 'fs'
import antlr4 from 'antlr4'
import VeDclLexer from '../grammar/VeDclLexer.js'
import VeDclParser from '../grammar/VeDclParser.js'
import VeDclListener from '../grammar/VeDclListener.js'
import {Dialog, buildTile} from './VeDclTiles.js'

class VeDclLoader extends VeDclListener {
    constructor(dclContext) {
        super()
        this.context = dclContext
    }

    get dialogs() {
        return Object
            .values(this.context.defines)
            .filter(obj => obj instanceof Dialog)
    }

    enterFile(_ctx) { }

    exitFile(_ctx) { }

    enterIncludeFile(ctx) {
        //console.log('enterIncludeFile');
        // Remove first and last double quotes (")
        const str = ctx.fileName().STR().getText()
        const filename = str.substring(1, str.length-1)
        load(filename, this.context)
    }

    exitIncludeFile(_ctx) {
        //console.log('exitIncludeFile');
        console.log(this.context)
    }

    enterDefineClusterTile(ctx) {
        //console.log('enterDefineClusterTile');
        const tileId = ctx.ID().getText()
        const tileName = ctx.clusterTile().getText()
        const tile = buildTile(tileName, tileId)
        this.context.defines[tileId] = tile
        this.context.clusters.push(tile)
        this.context.tiles.push(tile)
    }

    exitDefineClusterTile(_ctx) {
        //console.log('exitDefineClusterTile');
        this.context.clusters.pop()
        this.context.tiles.pop()
    }

    enterDefineSimpleTile(ctx) {
        //console.log('enterDefineSimpleTile');
        const tileId = ctx.ID().getText()
        const tileName = ctx.simpleTile().getText()
        const tile = buildTile(tileName, tileId)
        this.context.defines[tileId] = tile
        this.context.tiles.push(tile)
    }

    exitDefineSimpleTile(_ctx) {
        //console.log('exitDefineSimpleTile');
        this.context.tiles.pop()
    }

    enterInnerClusterTile(ctx) {
        //console.log('enterInnerClusterTile');
        const tileName = ctx.clusterTile().getText()
        const tile = buildTile(tileName)
        this.context.clusters.push(tile)
        this.context.tiles.push(tile)
    }

    exitInnerClusterTile(_ctx) {
        //console.log('exitInnerClusterTile');
        /*const cluster = */this.context.clusters.pop()
        this.context.clusters.top().addTile(this.context.tiles.pop())
    }

    enterInnerSimpleTile(ctx) {
        //console.log('enterInnerSimpleTile');
        const tileName = ctx.simpleTile().getText()
        const tile = buildTile(tileName)
        this.context.tiles.push(tile)
    }

    exitInnerSimpleTile(_ctx) {
        //console.log('exitInnerSimpleTile');
        this.context.clusters.top().addTile(this.context.tiles.pop())
    }

    enterInnerDeriveTile(ctx) {
        //console.log('enterInnerDeriveTile');
        const tileName = ctx.deriveTile().ID().getText()
        const tile = this.context.defines[tileName]
        if (!tile) {
            throw new Error(`Unknown tile: ${tileName}`)
        }
        const clone = tile.clone()
        this.context.tiles.push(clone)
    }

    exitInnerDeriveTile(_ctx) {
        //console.log('exitInnerDeriveTile');
        this.context.clusters.top().addTile(this.context.tiles.pop())
    }

    enterInnerAliasTile(ctx) {
        //console.log('enterInnerAliasTile');
        const tileName = ctx.aliasTile().ID().getText()
        const tile = this.context.defines[tileName]
        if (!tile) {
            throw new Error(`Unknown tile: ${tileName}`)
        }
        this.context.tiles.push(tile)
    }

    exitInnerAliasTile(_ctx) {
        //console.log('exitInnerAliasTile');
        this.context.clusters.top().addTile(this.context.tiles.pop())
    }

    enterAttribute(ctx) {
        const current = this.context.tiles.top()
        if (current) {
            const name = ctx.attributeName().ID().getText()
            let value = null
            if (ctx.attributeValue().BOOL()) {
                value = ctx.attributeValue().BOOL().getText() === 'true'
            } else if (ctx.attributeValue().INT()) {
                value = Number.parseInt(ctx.attributeValue().INT().getText())
            } else if (ctx.attributeValue().REAL()) {
                value = Number.parseFloat(ctx.attributeValue().REAL().getText())
            } else if (ctx.attributeValue().STR()) {
                // Remove first and last double quotes (")
                const str = ctx.attributeValue().STR().getText()
                value = str.substring(1, str.length-1)
            } else if (ctx.attributeValue().ALIGN()) {
                value = ctx.attributeValue().ALIGN().getText()
            } else if (ctx.attributeValue().LAYOUT()) {
                value = ctx.attributeValue().LAYOUT().getText()
            } else {
                throw new Error(`Unhandled: ${name} = ${ctx.attributeValue().getText()}`)
            }
            current.addAttribute(name, value)
        }
    }

    exitAttribute(_ctx) { }
}

function load(dclfile, context) {
    const dcl = readFile(dclfile)
    const {tree} = parseDcl(dcl)
    const loader = new VeDclLoader(context)
    const walker = new antlr4.tree.ParseTreeWalker()
    walker.walk(loader, tree)
    return loader.dialogs
}

function tree(dclfile) {
    const dcl = readFile(dclfile)
    const {parser, tree} = parseDcl(dcl)
    return tree.toStringTree(parser.ruleNames)
}

function readFile(file) {
    // TODO: handle errors
    return fs.readFileSync(file).toString()
}

function parseDcl(dcl) {
    const chars = new antlr4.InputStream(dcl)
    const lexer = new VeDclLexer(chars)
    const tokens = new antlr4.CommonTokenStream(lexer)
    const parser = new VeDclParser(tokens)
    //parser.removeErrorListeners();
    //parser.addErrorListener(new VeDclErrorListener());
    return {
        lexer,
        tokens,
        parser,
        tree: parser.file(),
    }
}

export {
    load,
    tree,
}

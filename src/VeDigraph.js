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

import {EOL} from 'os'

class VeDigraph {
    constructor(V) {
        this._vertices = V
        this._edges = 0
        this._adjacent = new Array(V)
        for (let v = 0; v < V; v++) {
            this._adjacent[v] = []
        }
    }

    vertices() {
        return this._vertices
    }

    edges() {
        return this._edges
    }

    addEdge(v, w) {
        this._adjacent[v].push(w)
        this._edges++
    }

    adjacent(v) {
        return this._adjacent[v]
    }

    toString() {
        let s = `${this._vertices} vertices, ${this._edges} edges` + EOL
        for (let v = 0; v < this._vertices; v++) {
            s += `${v}: `
            for (let w of this._adjacent[v]) {
                s += `${w} `
            }
            s += EOL
        }
        return s
    }

    toDot() {
        let s = 'digraph {' + EOL
        const nodes = new Set()
        for (let v = 0; v < this._vertices; v++) {
            for (let w of this._adjacent[v]) {
                nodes.add(v)
                nodes.add(w)
                s += `  ${v} -> ${w};` + EOL
            }
        }
        for (let v = 0; v < this._vertices; v++) {
            if (!nodes.has(v)) {
                s += `  ${v};` + EOL
            }
        }
        s += '}' + EOL
        return s
    }
}

export default VeDigraph

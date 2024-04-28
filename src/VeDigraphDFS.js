/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2024 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

class VeDigraphDFS {
    constructor(G, source) {
        this._marked = new Array(G.vertices())
        for (let v = 0; v < G.vertices(); v++) {
            this._marked[v] = false
        }
        let sources
        if (Symbol.iterator in Object(source)) {
            // Support any iterable
            sources = source
        } else {
            sources = [source]
        }
        for (let s of sources) {
            if (!this._marked[s]) {
                this._dfs(G, s)
            }
        }
    }

    _dfs(G, v) {
        this._marked[v] = true
        for (let w of G.adjacent(v)) {
            if (!this._marked[w]) {
                this._dfs(G, w)
            }
        }
    }

    hasPathTo(v) {
        return this._marked[v]
    }
}

export default VeDigraphDFS

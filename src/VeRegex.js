/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2020-2025 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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
import VeStack from './VeStack.js'
import VeDigraph from './VeDigraph.js'
import VeDigraphDFS from './VeDigraphDFS.js'

// See Algorithms, 4th Edition, 5.4 Regular Expressions for detail
class VeRegex {
    constructor(re) {
        const {G, RE} = VeRegex.epsilonTransitionDigraph(Array.from(re))
        this.G = G
        this.RE = RE
    }

    static epsilonTransitionDigraph(re) {
        const M = re.length
        const G = new VeDigraph(M+1)
        const RE = Array.from(re)
        const stack = new VeStack()
        for (let i = 0; i < M; i++) {
            let lp = i
            switch (re[i]) {
            case '(':
                //    i    i+1
                // -> ( -> ...
                //    -----^
                G.addEdge(i, i+1)
                // fall through
            case '|':
                stack.push(i)
                RE[i] = {
                    show: re[i]
                }
                break
            case ')': {
                //    i    i+1
                // -> ) -> ...
                //    -----^
                G.addEdge(i, i+1)
                const ors = new VeStack()
                for (;;) {
                    const j = stack.pop()
                    if (re[j] === '|') {
                        ors.push(j)
                    } else {
                        lp = j
                        break
                    }
                }
                while (!ors.isEmpty()) {
                    const or = ors.pop()
                    //    lp       or   or+1   i
                    // -> ( -> ... | -> ... -> ) ->
                    //    --------------^
                    //             ------------^
                    G.addEdge(lp, or+1)
                    G.addEdge(or, i)
                }
                RE[i] = {
                    show: re[i]
                }
                break
            }
            case '[': {
                RE[i] = undefined
                let j = i+1
                let negate = false
                let group = []

                // [^
                if (re[j] === '^') {
                    negate = true
                    j++
                }

                // [^- or [-
                if (re[j] === '-') {
                    group.push('-')
                    j++
                }

                while (j < M) {
                    // if range
                    if (re[j+1] === '-') {
                        // a-z
                        if (re[j+2] !== ']') {
                            const from = re[j].charCodeAt()
                            const to   = re[j+2].charCodeAt()
                            if (from <= to) {
                                // build range
                                for (let k = from; k <= to; k++) {
                                    group.push(String.fromCharCode(k))
                                }
                            } else {
                                // borders only
                                group.push(String.fromCharCode(from))
                                group.push(String.fromCharCode(to))
                            }
                            j += 3
                        // a-]
                        } else {
                            group.push(re[j])
                            group.push('-')
                            j += 2
                        }
                        continue
                    }

                    // if end
                    if (re[j] === ']' && re[j-1] !== '\\') {
                        if (negate) {
                            RE[j] = {
                                test: (x) => group.indexOf(x) === -1,
                                show: `[^${group.join('')}]`
                            }
                        } else {
                            RE[j] = {
                                test: (x) => group.indexOf(x) !== -1,
                                show: `[${group.join('')}]`
                            }
                        }
                        break
                    }

                    // otherwise
                    group.push(re[j])
                    j++
                }
                // TODO: RE[(i..j)] = undefined
                G.addEdge(i, j)
                i = j
                break
            }
            case '.':
                RE[i] = {
                    test: (_) => true,
                    show: 'any'
                }
                break
            case '\\': {
                RE[i] = undefined
                G.addEdge(i, i+1)
                const char = re[i+1]
                RE[i+1] = {
                    test: (x) => x === char,
                    show: char
                }
                i++ // escape handled too
                break
            }
            default: {
                const char = re[i]
                RE[i] = {
                    test: (x) => x === char,
                    show: char
                }
                break
            }}

            switch (re[i+1]) {
            case '?':
                //    lp   lp+1   i    i+1  i+2
                // -> ( -> ... -> ) -> ? -> ...
                //    -----^
                //    -----------------^
                //                     -----^
                G.addEdge(lp, lp+1)
                G.addEdge(lp, i+1)
                G.addEdge(i+1, i+2)
                RE[i+1] = {
                    show: re[i+1]
                }
                i++
                break
            case '*':
                //    lp          i    i+1  i+2
                // -> ( -> ... -> ) -> * -> ...
                //    -----------------^
                //    ^-----------------
                //                     -----^
                G.addEdge(lp, i+1)
                G.addEdge(i+1, lp)
                G.addEdge(i+1, i+2)
                RE[i+1] = {
                    show: re[i+1]
                }
                i++
                break
            case '+':
                //    lp          i    i+1  i+2
                // -> ( -> ... -> ) -> + -> ...
                //    ^-----------------
                //                     -----^
                G.addEdge(i+1, lp)
                G.addEdge(i+1, i+2)
                RE[i+1] = {
                    show: re[i+1]
                }
                i++
                break
            default:
                break
            }
        }
        return {
            G,
            RE,
        }
    }

    // :: (string) -> bool
    test(text) {
        let states = new Set()
        let dfs = new VeDigraphDFS(this.G, 0)
        for (let v = 0; v < this.G.vertices(); v++) {
            if (dfs.hasPathTo(v)) {
                states.add(v)
            }
        }

        const M = this.RE.length
        const N = text.length
        for (let i = 0; i < N; i++) {
            const char = text.charAt(i)
            const matches = new Set()
            for (let s of states) {
                if (s === M) {
                    continue
                }
                if (this.RE[s] && this.RE[s].test && this.RE[s].test(char)) {
                    matches.add(s + 1)
                }
            }

            states = new Set()
            dfs = new VeDigraphDFS(this.G, matches)
            for (let v = 0; v < this.G.vertices(); v++) {
                if (dfs.hasPathTo(v)) {
                    states.add(v)
                }
            }
        }

        for (let s of states) {
            if (s === M) {
                return true
            }
        }
        return false
    }

    // :: () -> string
    toDot() {
        let s = ''
        s += 'digraph {'
        s += EOL
        s += '  rankdir=LR'
        s += EOL

        for (let v = 0; v < this.G.vertices(); v++) {
            const ch = v < this.RE.length ? (this.RE[v] ? this.RE[v].show : ' ') : ' '
            s += '  ' + v + ' ' + this.attrs(this.label(ch), this.xlabel(v), this.shape('circle'))
            s += EOL
        }

        for (let v = 0; v < this.G.vertices()-1; v++) {
            if (!this.isMetaChar(this.RE[v] ? this.RE[v].show : ' ')) {
                s += '  ' + v + ' -> ' + (v+1) + ' ' + this.attrs(this.color('black'))
                s += EOL
            }
        }

        for (let v = 0; v < this.G.vertices(); v++) {
            for (let w of this.G.adjacent(v)) {
                s += '  ' + v + ' -> ' + w + ' ' + this.attrs(this.color('red'))
                s += EOL
            }
        }

        s += '}'
        s += EOL
        return s
    }

    isMetaChar(ch) {
        switch (ch) {
        case '(':
        case ')':
        case '*':
        case '+':
        case '|':
            return true
        default:
            return false
        }
    }

    attrs(...attrs) {
        return `[${attrs.join(', ')}]`
    }

    label(o) {
        return `label="${o}"`
    }

    xlabel(o) {
        return `xlabel="${o}"`
    }

    color(o) {
        return `color="${o}"`
    }

    shape(s) {
        return `shape="${s}"`
    }
}

export default VeRegex

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

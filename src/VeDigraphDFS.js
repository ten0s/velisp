class VeDigraphDFS {
    constructor(G, source) {
        this._marked = new Array(G.vertices())
        for (let v = 0; v < G.vertices(); v++) {
            this._marked[v] = false
        }
        let sources
        if (Array.isArray(source)) {
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

exports.VeDigraphDFS = VeDigraphDFS

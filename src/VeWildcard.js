const VeDigraph = require('./VeDigraph.js')
const VeDigraphDFS = require('./VeDigraphDFS.js')

// See Algorithms, 4th Edition, 5.4 Regular Expressions for detail
class VeWildcard {
    constructor(wc) {
        this.wc = Array.from(wc)
        this.G = this.epsilonTransitionDigraph()
    }

    epsilonTransitionDigraph() {
        const M = this.wc.length
        const G = new VeDigraph(M + 1)
        for (let i = 0; i < M; i++) {
            if (this.wc[i] === '*') {
                G.addEdge(i, i)
                G.addEdge(i, i + 1)
            }
        }
        return G
    }

    // :: (String) -> Boolean
    test(text) {
        let states = new Set()
        let dfs = new VeDigraphDFS(this.G, 0)
        for (let v = 0; v < this.G.vertices(); v++) {
            if (dfs.hasPathTo(v)) {
                states.add(v)
            }
        }

        const M = this.wc.length
        const N = text.length
        for (let i = 0; i < N; i++) {
            const char = text.charAt(i)
            const matches = new Set()
            for (let s of states) {
                if (s === M) {
                    continue
                }
                if (this.wc[s] === char || this.wc[s] === '?') {
                    matches.add(s + 1)
                }
                if (this.wc[s] === '*') {
                    matches.add(s)
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
}

module.exports = VeWildcard

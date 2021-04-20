const {VeDigraph} = require('./VeDigraph.js')
const {VeDigraphDFS} = require('./VeDigraphDFS.js')

// See Algorithms, 4th Edition, 5.4 Regular Expressions for detail
class VeGlob {
    constructor(glob) {
        this.glob = Array.from(glob.toLowerCase())
        this.G = this.epsilonTransitionDigraph()
    }

    epsilonTransitionDigraph() {
        const M = this.glob.length
        const G = new VeDigraph(M + 1)
        for (let i = 0; i < M; i++) {
            if (this.glob[i] === '*') {
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

        const M = this.glob.length
        const N = text.length
        for (let i = 0; i < N; i++) {
            const char = text.charAt(i).toLowerCase()
            const matches = new Set()
            for (let s of states) {
                if (s === M) {
                    continue
                }
                if (this.glob[s] === char || this.glob[s] === '?') {
                    matches.add(s + 1)
                }
                if (this.glob[s] === '*') {
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

exports.VeGlob = VeGlob

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
                switch (this.wc[s]) {
                case '#':
                    // range match
                    if (this.isDigit(char)) {
                        matches.add(s + 1)
                    }
                    break
                case '@':
                    // range match
                    if (this.isAlpha(char)) {
                        matches.add(s + 1)
                    }
                    break
                case '.':
                    // range match
                    if (!this.isAlpha(char) && !this.isDigit(char)) {
                        matches.add(s + 1)
                    }
                    break
                case '*':
                    // many matches
                    matches.add(s)
                    // any one match
                    matches.add(s + 1)
                    break
                case '?':
                    // any one match
                    matches.add(s + 1)
                    break
                case char:
                    // exact one match
                    matches.add(s + 1)
                    break
                default:
                    break
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

    isDigit(c) {
        return c >= '0' && c <= '9'
    }

    isAlpha(c) {
        return (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
    }
}

module.exports = VeWildcard

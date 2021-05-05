const VeStack = require('./VeStack.js')
const VeDigraph = require('./VeDigraph.js')
const VeDigraphDFS = require('./VeDigraphDFS.js')

// See Algorithms, 4th Edition, 5.4 Regular Expressions for detail
class VeRegex {
    constructor(re) {
        this.re = Array.from(re)
        this.G = this.epsilonTransitionDigraph()
    }

    epsilonTransitionDigraph() {
        const M = this.re.length
        const G = new VeDigraph(M+1)
        const stack = new VeStack()
        let ors = undefined
        for (let i = 0; i < M; i++) {
            let lp = i
            switch (this.re[i]) {
            case '(':
            case '|':
                stack.push(i)
                break
            case ')':
                ors = new VeStack()
                for (;;) {
                    const j = stack.pop()
                    if (this.re[j] === '|') {
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
                ors = undefined
                break
            default:
                break
            }

            if (i < M-1) {
                switch (this.re[i+1]) {
                case '*':
                    //    lp          i    i+1
                    // -> ( -> ... -> ) -> * ->
                    //    -----------------^
                    //    ^-----------------
                    G.addEdge(lp, i+1)
                    G.addEdge(i+1, lp)
                    break
                case '+':
                    //    lp          i    i+1
                    // -> ( -> ... -> ) -> + ->
                    //    ^-----------------
                    G.addEdge(i+1, lp)
                    break
                case '?':
                    //    lp   lp+1   i    i+1
                    // -> ( -> ... -> ) -> ? ->
                    //    -----^
                    //    -----------------^
                    G.addEdge(lp, lp+1)
                    G.addEdge(lp, i+1)
                    break
                default:
                    break
                }
            }
            switch (this.re[i]) {
            case '(':
            case ')':
            case '*':
            case '+':
            case '?':
                G.addEdge(i, i+1)
                break
            default:
                break
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

        const M = this.re.length
        const N = text.length
        for (let i = 0; i < N; i++) {
            const char = text.charAt(i)
            const matches = new Set()
            for (let s of states) {
                if (s === M) {
                    continue
                }
                switch (this.re[s]) {
                case '.':
                    matches.add(s + 1)
                    break
                case char:
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
}

module.exports = VeRegex

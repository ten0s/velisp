const QUnit = require('qunit')
const {VeDigraph} = require('../src/VeDigraph.js')

QUnit.test('VeDigraph', assert => {
    const G = new VeDigraph(10)
    assert.equal(10, G.vertices())
    assert.equal(0, G.edges())

    G.addEdge(0, 1)
    G.addEdge(0, 7)
    G.addEdge(1, 2)
    G.addEdge(1, 3)
    G.addEdge(1, 7)
    G.addEdge(1, 8)
    G.addEdge(2, 3)
    G.addEdge(3, 4)
    G.addEdge(3, 5)
    G.addEdge(3, 8)
    G.addEdge(4, 5)
    G.addEdge(5, 6)
    G.addEdge(5, 7)
    G.addEdge(5, 8)
    G.addEdge(6, 7)
    G.addEdge(7, 8)

    //console.log(G)
    //console.log(G.toString())
    //console.log(G.toDot())

    assert.equal(10, G.vertices())
    assert.equal(16, G.edges())

    let outdegree0 = 0
    G.adjacent(0).forEach(_ => outdegree0++)
    assert.equal(2, outdegree0)
    
    let outdegree1 = 0
    G.adjacent(1).forEach(_ => outdegree1++)
    assert.equal(4, outdegree1)

    let outdegree8 = 0
    G.adjacent(8).forEach(_ => outdegree8++)
    assert.equal(0, outdegree8)
})

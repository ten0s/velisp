const {VeStack} = require('./VeStack.js')

class VeDclContext {
    constructor() {
        this.defines = {}
        this.clusters = new VeStack()
        this.tiles = new VeStack()
    }
}

exports.VeDclContext = VeDclContext

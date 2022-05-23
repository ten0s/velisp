import VeStack from './VeStack.js'

class VeDclContext {
    constructor() {
        this.defines = {}
        this.clusters = new VeStack()
        this.tiles = new VeStack()
    }
}

export default VeDclContext

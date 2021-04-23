const VeLispContext = require('./VeLispContext.js')
const Kernel = require('./kernel/Kernel.js')

class VeLispGlobalContext extends VeLispContext {
    constructor() {
        super()
        Kernel.initContext(this)
    }
}

module.exports = VeLispGlobalContext

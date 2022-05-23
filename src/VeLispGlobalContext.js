import VeLispContext from './VeLispContext.js'
import * as Kernel from './kernel/Kernel.js'

class VeLispGlobalContext extends VeLispContext {
    constructor() {
        super()
        Kernel.initContext(this)
    }
}

export default VeLispGlobalContext

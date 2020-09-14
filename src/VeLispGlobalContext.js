const {Context} = require('./VeLispContext.js');
const Kernel = require('./kernel/Kernel.js');

class GlobalContext extends Context {
    constructor() {
        super();
        Kernel.addTo(this);
    }
}

exports.GlobalContext = GlobalContext;

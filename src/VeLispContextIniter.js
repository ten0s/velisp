import {evaluate} from './VeLispEvaluator.js'
import * as Kernel from './kernel/Kernel.js'

class VeLispContextIniter {
    static initWithKernel(context) {
        Kernel.initContext(context)
    }

    static initWithLib(context) {
        let rootdir = process.env['VELISP_ROOT']
        evaluate(`(load "${rootdir}/lib/main.lsp")`, context)
    }

    static async initWithDcl(context) {
        const DCL = await import('./kernel/DCL.js')
        DCL.initContext(context)
    }
}

export default VeLispContextIniter

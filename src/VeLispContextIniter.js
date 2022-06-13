import __rootdir from './VeRootDir.js'
import {fixWinPath} from './VeUtil.js'
import {evaluate} from './VeLispEvaluator.js'
import * as Kernel from './kernel/Kernel.js'

class VeLispContextIniter {
    static initWithKernel(context) {
        Kernel.initContext(context)
    }

    static initWithLib(context) {
        const rootdir = fixWinPath(__rootdir)
        process.env['VELISP_ROOT'] = rootdir
        evaluate(`(load "${rootdir}/lib/main.lsp")`, context)
    }

    static async initWithDcl(context) {
        const DCL = await import('./kernel/DCL.js')
        DCL.initContext(context)
    }
}

export default VeLispContextIniter

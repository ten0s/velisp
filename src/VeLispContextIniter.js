import path from 'path'
import __rootdir from './VeRootDir.js'
import {fixWinPath} from './VeUtil.js'
import {evaluate} from './VeLispEvaluator.js'
import * as Kernel from './kernel/Kernel.js'

class VeLispContextIniter {
    static initWithKernel(context) {
        Kernel.initContext(context)
    }

    static initWithLib(context) {
        let rootdir = fixWinPath(__rootdir)
        if (rootdir.startsWith('/snapshot')) {
            rootdir = fixWinPath(path.dirname(process.argv[0]))
        }
        process.env['VELISP_ROOT'] = rootdir
        evaluate(`(load "${rootdir}/lib/main.lsp")`, context)
    }

    static async initWithDcl(context) {
        // initWithLib should be called first
        // since it sets VELISP_ROOT used later
        // to inject lib/dcl/{base,acad}.dcl
        const DCL = await import('./kernel/DCL.js')
        DCL.initContext(context)
    }
}

export default VeLispContextIniter

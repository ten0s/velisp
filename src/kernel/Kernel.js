import * as Application from './Application.js'
import * as Arithmetic from './Arithmetic.js'
import * as Consts from './Consts.js'
import * as DCL from './DCL.js'
import * as Equality from './Equality.js'
import * as Function from './Function.js'
import * as IO from './IO.js'
import * as List from './List.js'
import * as Math from './Math.js'
import * as String from './String.js'
import * as Symbol from './Symbol.js'
import * as System from './System.js'
import * as VEFileName from './VE-Filename.js'
import * as VEFileSystem from './VE-FileSystem.js'
import * as VLFileName from './VL-Filename.js'
import * as VLFileSystem from './VL-FileSystem.js'
import * as VLList from './VL-List.js'
import * as VLString from './VL-String.js'
import * as VLSymbol from './VL-Symbol.js'

export const initContext = (context) => {
    Application.initContext(context)
    Arithmetic.initContext(context)
    Consts.initContext(context)
    DCL.initContext(context)
    Equality.initContext(context)
    Function.initContext(context)
    IO.initContext(context)
    List.initContext(context)
    Math.initContext(context)
    String.initContext(context)
    Symbol.initContext(context)
    System.initContext(context)
    VEFileName.initContext(context)
    VEFileSystem.initContext(context)
    VLFileName.initContext(context)
    VLFileSystem.initContext(context)
    VLList.initContext(context)
    VLString.initContext(context)
    VLSymbol.initContext(context)
}

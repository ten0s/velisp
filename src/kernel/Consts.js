import {EOL} from 'os'
import {Bool, Real, Str} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('T', new Bool(true))
    context.setSym('PI', new Real(Math.PI))
    // VeLisp Extension
    context.setSym('EOL', new Str(EOL))
}

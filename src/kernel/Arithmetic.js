import {Int, Real, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('*', new Fun('*', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0)
        }
        let result = new Int(1)
        for (let i = 0; i < args.length; i++) {
            result = result.multiply(ensureType('*:', args[i], [Int, Real]))
        }
        return result
    }))
    context.setSym('/', new Fun('/', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0)
        }
        let result = ensureType('/:', args[0], [Int, Real])
        for (let i = 1; i < args.length; i++) {
            result = result.divide(ensureType('/:', args[i], [Int, Real]))
        }
        return result
    }))
    context.setSym('+', new Fun('+', ['[num] ...'], [], (self, args) => {
        let result = new Int(0)
        for (let i = 0; i < args.length; i++) {
            result = result.add(ensureType('+:', args[i], [Int, Real]))
        }
        return result
    }))
    context.setSym('-', new Fun('-', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0)
        }
        let result = ensureType('-:', args[0], [Int, Real])
        if (args.length === 1) {
            return result.multiply(new Int(-1))
        }
        for (let i = 1; i < args.length; i++) {
            result = result.subtract(ensureType('-:', args[i], [Int, Real]))
        }
        return result
    }))
    context.setSym('~', new Fun('~', ['int'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('~: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('~: too many arguments')
        }
        return ensureType('~:', args[0], [Int]).bitwiseNot()
    }))
}

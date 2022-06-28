/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

/* SPDX-License-Identifier: GPL-3.0-or-later */

import {Int, Real, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('ATAN', new Fun('atan', ['num1', '[num2]'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('atan: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('atan: too many arguments')
        }
        let num1 = ensureType('atan:', args[0], [Int, Real])
        if (args.length === 2) {
            let num2 = ensureType('atan:', args[1], [Int, Real])
            if (num2.value() === 0) {
                return new Real(Math.PI / 2)
            }
            num1 = num1.divide(num2)
        }
        return new Real(Math.atan(num1.value()))
    }))
    context.setSym('COS', new Fun('cos', ['ang'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('cos: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('cos: too many arguments')
        }
        const ang = ensureType('cos:', args[0], [Int, Real]).value()
        return new Real(Math.cos(ang))
    }))
    context.setSym('EXP', new Fun('exp', ['num'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('exp: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('exp: too many arguments')
        }
        const num = ensureType('exp:', args[0], [Int, Real]).value()
        return new Real(Math.exp(num))
    }))
    context.setSym('EXPT', new Fun('expt', ['num', 'power'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('expt: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('expt: too many arguments')
        }
        const num = ensureType('expt: `num`', args[0], [Int, Real])
        const power = ensureType('expt: `power`', args[1], [Int, Real])
        const isReal = num instanceof Real || power instanceof Real
        const result = Math.pow(num.value(), power.value())
        if (isReal) {
            return new Real(result)
        }
        return new Int(result)
    }))
    context.setSym('FIX', new Fun('fix', ['num'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('fix: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('fix: too many arguments')
        }
        const num = ensureType('fix:', args[0], [Int, Real]).value()
        return new Int(Math.floor(num))
    }))
    context.setSym('LOG', new Fun('log', ['num'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('log: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('log: too many arguments')
        }
        const num = ensureType('log:', args[0], [Int, Real]).value()
        if (num <= 0) {
            throw new Error('log: expected positive Int, Real')
        }
        return new Real(Math.log(num))
    }))
    context.setSym('MAX', new Fun('max', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0)
        }
        let max = ensureType('max:', args[0], [Int, Real])
        let isReal = max instanceof Real
        for (let i = 1; i < args.length; i++) {
            const num = ensureType('max:', args[i], [Int, Real])
            isReal = isReal || num instanceof Real
            if (!max.lessThan(num).isNil()) {
                max = num
            }
        }
        if (isReal) {
            return new Real(max.value())
        }
        return max
    }))
    context.setSym('MIN', new Fun('min', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0)
        }
        let min = ensureType('min:', args[0], [Int, Real])
        let isReal = min instanceof Real
        for (let i = 1; i < args.length; i++) {
            const num = ensureType('min:', args[i], [Int, Real])
            isReal = isReal || num instanceof Real
            if (!num.lessThan(min).isNil()) {
                min = num
            }
        }
        if (isReal) {
            return new Real(min.value())
        }
        return min
    }))
    context.setSym('REM', new Fun('rem', ['[num] ...'], [], (self, args) => {
        if (args.length === 0) {
            return new Int(0)
        }
        let rem = ensureType('rem:', args[0], [Int, Real])
        for (let i = 1; i < args.length; i++) {
            const num = ensureType('rem:', args[i], [Int, Real])
            rem = rem.remainder(num)
        }
        return rem
    }))
    context.setSym('SIN', new Fun('sin', ['ang'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('sin: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('sin: too many arguments')
        }
        const ang = ensureType('sin:', args[0], [Int, Real]).value()
        return new Real(Math.sin(ang))
    }))
    context.setSym('SQRT', new Fun('sqrt', ['num'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('sqrt: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('sqrt: too many arguments')
        }
        const num = ensureType('sqrt:', args[0], [Int, Real]).value()
        if (num < 0) {
            throw new Error('sqrt: expected positive Int, Real')
        }
        return new Real(Math.sqrt(num))
    }))
}

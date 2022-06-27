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

import {Bool, Fun} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('=', new Fun('=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('=: too few arguments')
        }
        let result = new Bool(true)
        let val1 = args[0]
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i]
            result = val1.equalTo(val2)
            if (result.isNil()) break
            val1 = val2
        }
        return result
    }))
    context.setSym('/=', new Fun('/=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('/=: too few arguments')
        }
        let result = new Bool(true)
        let val1 = args[0]
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i]
            result = val1.equalTo(val2).not()
            if (result.isNil()) break
            val1 = val2
        }
        return result
    }))
    context.setSym('<', new Fun('<', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('<: too few arguments')
        }
        let result = new Bool(true)
        let val1 = args[0]
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i]
            result = val1.lessThan(val2)
            if (result.isNil()) break
            val1 = val2
        }
        return result
    }))
    context.setSym('<=', new Fun('<=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('<=: too few arguments')
        }
        let result = new Bool(true)
        let val1 = args[0]
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i]
            result = val1.lessThan(val2).or(val1.equalTo(val2))
            if (result.isNil()) break
            val1 = val2
        }
        return result
    }))
    context.setSym('>', new Fun('>', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('>: too few arguments')
        }
        let result = new Bool(true)
        let val1 = args[0]
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i]
            result = val1.lessThan(val2).or(val1.equalTo(val2)).not()
            if (result.isNil()) break
            val1 = val2
        }
        return result
    }))
    context.setSym('>=', new Fun('>=', ['numstr [numstr] ...'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('>=: too few arguments')
        }
        let result = new Bool(true)
        let val1 = args[0]
        for (let i = 1; i < args.length; i++) {
            const val2 = args[i]
            result = val1.lessThan(val2).not()
            if (result.isNil()) break
            val1 = val2
        }
        return result
    }))
    context.setSym('EQ', new Fun('eq', ['expr1 expr2'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('eq: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('eq: too many arguments')
        }
        // Referencial Equality
        const val1 = args[0]
        const val2 = args[1]
        return val1.eq(val2)
    }))
    context.setSym('EQUAL', new Fun('equal', ['expr1 expr2'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('equal: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('equal: too many arguments')
        }
        // Structural Equality
        const val1 = args[0]
        const val2 = args[1]
        return val1.equal(val2)
    }))
}

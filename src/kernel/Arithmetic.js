/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2023 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

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

class VeStack {
    constructor() {
        this.stack = []
    }

    // :: () -> bool
    isEmpty() {
        return this.stack.length === 0
    }

    // :: () -> non_neg_int
    size() {
        return this.stack.length
    }

    // :: (value) -> int
    push(value) {
        this.stack.push(value)
    }

    // :: () -> value | throw Error
    pop() {
        if (this.stack.length > 0) {
            return this.stack.pop()
        }
        throw new Error('Stack is empty')
    }

    // :: (int) -> value | throw Error
    top(n = 0) {
        if (this.stack.length > n) {
            return this.stack[this.stack.length-1-n]
        }
        throw new Error('Stack is empty')
    }
}

export default VeStack

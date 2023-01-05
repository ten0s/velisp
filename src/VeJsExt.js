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

// :: ((any) -> bool) -> ((any) -> bool)
const not = func => (x) => !(func(x))

// :: ([x], x) -> bool
const contains = xs => x => xs.includes(x)

if (!Array.prototype.with) {
    Array.prototype.with = function vvith(xs = []) {
        if (!Array.isArray(xs)) {
            xs = [xs]
        }
        return this.filter(contains(xs))
    }
}

if (!Array.prototype.without) {
    Array.prototype.without = function without(xs = []) {
        if (!Array.isArray(xs)) {
            xs = [xs]
        }
        return this.filter(not(contains(xs)))
    }
}

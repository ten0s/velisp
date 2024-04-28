/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2024 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

import VeRegex from './VeRegex.js'

// :: (char) -> bool
const isalpha = (c) => {
    return (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
}

class VeGlob {
    constructor(glob) {
        this._glob = glob
        this._re = this._regex(glob)
        this._Regex = new VeRegex(this._re)
    }

    // :: (string) -> bool
    test(text) {
        return this._Regex.test(text)
    }

    // :: (string) -> string
    _regex(wc) {
        const re = []
        for (let i = 0; i < wc.length; i++) {
            switch(wc[i]) {
            case '?':
                re.push('.')
                break
            case '*':
                re.push('.*')
                break
            case '.':
                re.push('\\.')
                break
            default:
                if (isalpha(wc[i])) {
                    re.push(`[${wc[i].toLowerCase()}${wc[i].toUpperCase()}]`)
                } else {
                    re.push(wc[i])
                }
                break
            }
        }
        return re.join('')
    }

    // :: () -> string
    toRegex() {
        return this._re
    }

    // :: () -> string
    toDot() {
        return this._Regex.toDot()
    }
}

export default VeGlob

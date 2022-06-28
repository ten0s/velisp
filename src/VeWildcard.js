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

import VeRegex from './VeRegex.js'

class VeWildcard {
    constructor(wc) {
        this._wc = wc
        this._wcs = this._splitByComma(wc)
        this._res = this._wcs.map(wc => {
            const re = this._regex(wc)
            return {
                negate: re.negate,
                regex: re.regex,
                Regex: new VeRegex(re.regex),
            }
        })
    }

    // :: (string) -> bool
    test(text) {
        return this._res.some(re => {
            const res = re.Regex.test(text)
            if (re.negate) {
                return !res
            }
            return res
        })
    }

    // :: (string) -> {negate: bool, regex: string}
    _regex(wc) {
        let i = 0
        let negate = false
        if (wc[0] === '~') {
            negate = true
            i++
        }
        const re = []
        let insideGroup = false
        for (; i < wc.length; i++) {
            switch(wc[i]) {
            case '[':
                insideGroup = true
                re.push(wc[i])
                break
            case ']':
                insideGroup = false
                re.push(wc[i])
                break

            case '`':  // escape
                if (wc[i+1] === '`') {
                    re.push('`')
                } else {
                    re.push('\\')
                    re.push(wc[i+1])
                }
                i++
                break
            case '#':
                if (insideGroup) {
                    re.push('#')
                } else {
                    re.push('[0-9]')
                }
                break
            case '@':
                if (insideGroup) {
                    re.push('@')
                } else {
                    re.push('[a-zA-Z]')
                }
                break
            case '.':
                if (insideGroup) {
                    re.push('.')
                } else {
                    re.push('[^a-zA-Z0-9]')
                }
                break
            case '?':
                if (insideGroup) {
                    re.push('?')
                } else {
                    re.push('.')
                }
                break
            case '*':
                if (insideGroup) {
                    re.push('*')
                } else {
                    re.push('.*')
                }
                break
            case '~':
                if (wc[i-1] === '[') { // [~
                    re.push('^')
                } else {
                    re.push('~')
                }
                break
            case '^':  // literal ^
                re.push('\\^')
                break
            default:
                re.push(wc[i])
                break
            }
        }
        return {
            negate,
            regex: re.join(''),
        }
    }

    // :: (string) -> [string]
    _splitByComma(str) {
        const out = []
        const arr = Array.from(str)
        let acc = []
        let insideGroup = false
        for (let i = 0; i < arr.length; i++) {
            if (arr[i] === '[' && arr[i-1] !== '`') {
                insideGroup = true
            }
            if (arr[i] === ']' && arr[i-1] !== '`') {
                insideGroup = false
            }
            if (arr[i] === ',' && !insideGroup && arr[i-1] !== '`') {
                out.push(acc.join(''))
                acc = []
            } else {
                acc.push(arr[i])
            }
        }
        out.push(acc.join(''))
        return out
    }

    // :: () -> string
    toRegex() {
        return this._res.map(re => {
            return re.negate ? `<|NOT|> ${re.regex}` : re.regex
        }).join(' <|OR|> ')
    }

    // :: () -> string
    toDot() {
        return this._res.map(re => re.Regex.toDot()).join('\n')
    }
}

export default VeWildcard

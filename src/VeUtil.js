/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2020-2025 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

import path from 'path'
import util from 'util'
import VeStack from './VeStack.js'

// :: (any, [any]) -> bool
const find = (y, xs) => {
    for (let x of xs) {
        if (y === x) {
            return true
        }
    }
    return false
}

// :: (string) -> string
const escape = (s) => {
    return s
        .replaceAll('\\'    , '\\\\')
        .replaceAll('"'     , '\\"')
        .replaceAll('\r'    , '\\r')
        .replaceAll('\n'    , '\\n')
        .replaceAll('\t'    , '\\t')
        .replaceAll('\u001b', '\\e')
}

// :: (string) -> string
const unescape = (s) => {
    const a = Array.from(s)
    //console.log(a)
    let stop = false
    const b = []
    for (let i = 0; i < a.length && !stop; i++) {
        // See VeLisp.g4 ESCAPE_SEQ
        if (a[i] === '\\') {
            switch (a[i+1]) {
            case '\\':
                b.push('\\')
                i++
                break
            case '"':
                b.push('"')
                i++
                break
            case 'r':
                b.push('\r')
                i++
                break
            case 'n':
                b.push('\n')
                i++
                break
            case 't':
                b.push('\t')
                i++
                break
            case 'e':
                b.push('\u001b')
                i++
                break
            case '0':
                // simulate null char behavior
                stop = true
                break
            default:
                b.push(a[i+1])
                i++
                break
            }
        } else {
            b.push(a[i])
        }
    }
    return b.join('')
}

// TODO: FIXME
// Poor man's replaceAll
if (!String.prototype.replaceAll) {
    String.prototype.replaceAll = function replaceAll(from, to) {
        return this.split(from).join(to)
    }
}

const inspect = (obj) => {
    // https://nodejs.org/api/util.html#util_util_inspect_object_showhidden_depth_colors
    return util.inspect(obj, {
        showHidden: false,
        depth: null,
        colors: true,
    })
}

// :: (string) -> bool
// Determine if the given string is recoverable by adding
// one or more right parentheses.
// * Left parenthesis should have a corresponding right parenthesis
// * Parentheses inside double quotes should be ignored
// * Given string should be incomplete
const isRecoverableInput = (s) => {
    const stack = new VeStack()
    let inQuotes = false
    for (let i = 0; i < s.length; i++) {
        const c = s.charAt(i)
        switch (c) {
        case '(':
            if (!inQuotes) {
                stack.push(c)
            }
            break
        case ')':
            if (!inQuotes) {
                if (stack.isEmpty() || stack.pop() != '(') {
                    return false
                }
            }
            break
        case '"':
            if (inQuotes) {
                if (i > 0 && s.charAt(i - 1) != '\\') {
                    inQuotes = !inQuotes
                }
            } else {
                inQuotes = true
            }
            break
        default:
            break
        }
    }
    if (stack.isEmpty()) {
        return false
    }
    while (!stack.isEmpty()) {
        if (stack.pop() != '(') {
            return false
        }
    }
    return true
}

// :: (string, string) -> string
const ensureExt = (name, ext) => {
    if (path.extname(name)) {
        // there's some extention
        return name
    }
    return name + ext
}

// :: (string) -> string
const ensureLspExt = (name) => {
    return ensureExt(name, '.lsp')
}

// :: (string) -> string
const ensureDclExt = (name) => {
    return ensureExt(name, '.dcl')
}

// :: (string) -> string
const ensureSldExt = (name) => {
    return ensureExt(name, '.sld')
}

// :: (string) -> string
const ensureSlbExt = (name) => {
    return ensureExt(name, '.slb')
}

// :: (string) -> string
const makeUnixPath = (path) => {
    // Win32 workaround
    return path.replaceAll('\\', '/')
}

// :: (string) -> string
const makeWinPath = (path) => {
    return path.replaceAll('/', '\\')
}

export {
    find,
    escape,
    unescape,
    inspect,
    isRecoverableInput,
    ensureLspExt,
    ensureDclExt,
    ensureSldExt,
    ensureSlbExt,
    makeUnixPath,
    makeWinPath,
}

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

import {Bool, Sym, List, Fun, KFun} from '../VeLispTypes.js'

const _traces = new Set()

// :: (string) -> boolean
export const isTrace = (name) => {
    return _traces.has(name.toUpperCase())
}

// :: (string) -> ()
const addTrace = (name) => {
    _traces.add(name.toUpperCase())
}

// :: (string) -> ()
const rmTrace = (name) => {
    _traces.delete(name.toUpperCase())
}

// :: () -> ()
const rmTraces = () => {
    _traces.clear()
}

// :: ([string]) -> List(Sym)
const traceNames = (names) => {
    return new List(names.sort().map(name => new Sym(name)))
}

// :: () -> List(Sym)
const allTraceNames = () => {
    return traceNames(Array.from(_traces.values()))
}

export const initContext = (context) => {
    context.setSym('TRACE', new KFun('trace', ['[function] ...'], [], (self, args) => {
        if (args.length === 0) {
            return allTraceNames()
        }
        let arg = new Bool(false)
        const names = []
        for (let i = 0; i < args.length; i++) {
            arg = args[i]
            switch (true) {
            case arg instanceof Sym: {
                const name = arg.value()
                if (!isTrace(name)) {
                    addTrace(name)
                    names.push(name)
                }
                break
            }
            case arg instanceof Fun: {
                const name = arg.name
                if (!isTrace(name)) {
                    addTrace(name)
                    names.push(name)
                }
                break
            }
            case arg.isNil():
                break
            default:
                throw new Error('trace: `function` expected Sym, Fun')
            }
        }
        // Return added traces
        return traceNames(names)
    }))
    context.setSym('UNTRACE', new KFun('untrace', ['[function] ...'], [], (self, args) => {
        if (args.length === 0) {
            const names = allTraceNames()
            rmTraces()
            return names
        }
        let arg = new Bool(false)
        const names = []
        for (let i = 0; i < args.length; i++) {
            arg = args[i]
            switch (true) {
            case arg instanceof Sym: {
                const name = arg.value()
                if (isTrace(name)) {
                    rmTrace(name)
                    names.push(name)
                }
                break
            }
            case arg instanceof Fun: {
                const name = arg.name
                if (isTrace(name)) {
                    rmTrace(name)
                    names.push(name)
                }
                break
            }
            case arg.isNil():
                break
            default:
                throw new Error('untrace: `function` expected Sym, Fun')
            }
        }
        // Return removed names
        return traceNames(names)
    }))
}

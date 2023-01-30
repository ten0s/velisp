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

import {Bool} from './VeLispTypes.js'

// VeLisp is a Lisp-1
class VeLispContext {
    constructor(parent = null) {
        // Binding context
        this.parent = parent
        this.symbols = {}
        // Call frame
        this.funName = null
        this.funFile = null
        this.callerName = null
        this.callerFile = null
        this.callerLine = null
    }

    // Set variable into the current context only.
    // :: (string, Type) -> ()
    setTopVar(name, value) {
        //console.log(`setTopVar(${name}, ${value})`, this);
        this.symbols[name] = value
    }

    // Get variable from the current context only, if defined.
    // Otherwise, return nil.
    // :: (string) -> Type | Nil
    getTopVar(name) {
        const value = this.symbols[name]
        if (value) {
            return value
        }
        return new Bool(false)
    }

    // Check if variable is defined in current context only.
    // :: (string) -> boolean
    isTopVar(name) {
        return !!this.symbols[name]
    }

    // Delete variable from the current context only.
    // :: (string) -> ()
    delTopVar(name) {
        delete this.symbols[name]
    }

    // Set variable into the current context, if defined.
    // If not defined, set into the parent context, if available.
    // Otherwise, set into the current (global) context.
    // :: (string, Type) -> ()
    setVar(name, value) {
        //console.log(`setVar(${name}, ${value})`, this);
        if (this.symbols[name]) {
            this.symbols[name] = value
        } else if (this.parent) {
            this.parent.setVar(name, value)
        } else {
            this.symbols[name] = value
        }
    }

    // Get variable from the current context, if defined.
    // If not defined, get variable from the parent context, if available.
    // Otherwise, return nil.
    // :: (string) -> Type | Nil
    getVar(name) {
        const value = this.symbols[name]
        if (value) {
            return value
        }
        if (this.parent) {
            return this.parent.getVar(name)
        }
        return new Bool(false)
    }

    // Set symbol into the current context, if defined.
    // If not defined, set into the parent context, if available.
    // Otherwise, set into the current (global) context.
    // :: (string, Type) -> ()
    setSym(name, value) {
        this.setVar(name, value)
    }

    // Get symbol from the current context, if defined.
    // If not defined, get symbol from the parent context, if available.
    // Otherwise, return nil.
    // :: (string) -> Type | Nil
    getSym(name) {
        return this.getVar(name)
    }

    // :: () -> [string]
    getSyms() {
        let syms = Object.keys(this.symbols).map(s => s.toUpperCase())
        if (this.parent) {
            syms = syms.concat(this.parent.getSyms())
        }
        return syms
    }
}

export default VeLispContext

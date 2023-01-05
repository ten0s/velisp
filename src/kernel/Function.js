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

import {Sym, List, Fun} from '../VeLispTypes.js'

export const initContext = (context) => {
    context.setSym('APPLY', new Fun('apply', ['function', 'list'], [], (self, args) => {
        //console.log('apply args', args);
        if (args.length < 2) {
            throw new Error('apply: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('apply: too many arguments')
        }
        let fun = args[0]
        if (fun instanceof Sym) {
            // Try resolving symbol to function
            fun = self.contexts[self.contexts.length-1].getSym(fun.value())
        }
        if (fun instanceof Fun) {
            let list = args[1]
            if (list instanceof List) {
                list = list.value()
            } else if (list.isNil()) {
                list = []
            } else {
                throw new Error('apply: `list` expected List')
            }
            return fun.apply(self, list)
        }
        throw new Error(`apply: no such function ${args[0]}`)
    }))
}

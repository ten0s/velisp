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

import path from 'path'
import {Sym, Str, List, Pair, Fun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    // VeLisp Extension
    context.setSym('FILENAME-PARSE', new Fun('filename-parse', ['filename'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('filename-parse: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('filename-parse: too many arguments')
        }
        const filename = ensureType('filename-parse:', args[0], [Str]).value()
        const parsed = path.win32.parse(filename)
        return new List([
            new Pair(new Sym('root'), new Str(parsed['root'])),
            new Pair(new Sym('dir'),  new Str(parsed['dir'])),
            new Pair(new Sym('base'), new Str(parsed['base'])),
            new Pair(new Sym('name'), new Str(parsed['name'])),
            new Pair(new Sym('ext'),  new Str(parsed['ext'])),
        ])
    }))
}

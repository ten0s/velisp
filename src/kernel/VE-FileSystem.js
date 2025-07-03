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

import fs from 'fs'
import {Bool, Str, KFun, ensureType} from '../VeLispTypes.js'

export const initContext = (context) => {
    // VeLisp Extension
    context.setSym('RMDIR', new KFun('rmdir', ['dirname'], [], (self, args) => {
        if (args.length === 0) {
            throw new Error('rmdir: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('rmdir: too many arguments')
        }
        const dirname = ensureType('rmdir:', args[0], [Str]).value()
        try {
            fs.rmdirSync(dirname)
            return new Bool(true)
        } catch /*(e)*/ {
            // TODO: put to *error*?
            // console.error(e)
            return new Bool(false)
        }
    }))
}

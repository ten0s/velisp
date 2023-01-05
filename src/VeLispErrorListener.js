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

import antlr4 from 'antlr4'
import {makeError} from './VeLispError.js'

class VeLispErrorListener extends antlr4.error.ErrorListener {
    constructor(context) {
        super()
        this.context = context
    }
    syntaxError(recognizer, symbol, line, column, message, _payload) {
        throw new Error(makeError(
            `line: ${line} column: ${column} message: ${message}`,
            this.context
        ))
    }
}

export default VeLispErrorListener

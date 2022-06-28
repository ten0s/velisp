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

import path from 'path'
import {Str} from './VeLispTypes.js'

function makeError(message, context) {
    let file = context.getVar('%VELISP_LSP_FILE%')
    if (file instanceof Str) {
        file = path.basename(file.value())
        return `file: ${file} ${message}`
    }
    return message
}

function fmtError(name, error) {
    //console.error(error);
    let message = `${name}: `
    if (error.path)  {
        message += `${error.path}: `
    }
    if (error.code)  {
        message += `${perror(error.code)}`
    } else {
        message += `${error.message}`
    }
    return message
}

function perror(errCode) {
    switch(errCode) {
    case 'EACCES':
        return 'Permission denied'
    case 'EEXIST':
        return 'File exists'
    case 'EISDIR':
        return 'Is a directory'
    case 'EMFILE':
        return 'Too many open files in system'
    case 'ENOENT':
        return 'No such file or directory'
    case 'ENOTDIR':
        return 'Not a directory'
    case 'ETIMEDOUT':
        return 'Operation timed out'
    default:
        return errCode
    }
}

export {
    makeError,
    fmtError,
}

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

import path from 'path'
import {Int, Str} from './VeLispTypes.js'
import VeSysInfo from './VeSysInfo.js'

function makeError(message, context) {
    let file = context.getVar('%VELISP_LSP_FILE%')
    let line = context.getVar('%VELISP_LSP_LINE%')
    let desc = ''
    if (file instanceof Str) {
        file = path.basename(file.value())
        desc = file
        if (line instanceof Int) {
            line = line.value()
            desc = `${file}:${line}`
        }
    }
    if (desc.length != 0) {
        return `location: ${desc} message: ${message}`
    } else {
        return `message: ${message}`
    }
}

function fmtError(name, error) {
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

function catchError(func, onError, context) {
    try {
        return func()
    } catch (e) {
        onError(e, context)
    }
}

function printError(error, context) {
    if (VeSysInfo.debug.stacktrace) {
        error.message = makeError(error.message, context)
        console.error(error)
    } else {
        console.error('Error: ' + makeError(error.message, context))
    }
}

export {
    fmtError,
    catchError,
    printError,
}

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
import {EOL} from 'os'
import VeSysInfo from './VeSysInfo.js'

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

function catchError(fun, onError, stack) {
    try {
        return fun()
    } catch (e) {
        onError(e, stack)
    }
}

function makeTrace(stack) {
    const cwd = process.cwd()
    return stack.fold((acc, frame, i) => {
        //console.log('--------------------', i)
        if (frame.funName) {
            const name = stack.top(i+1).funName
            const file = path.relative(cwd, frame.callerFile)
            const line = frame.callerLine
            if (name) {
                acc.push(`    at ${name} (${file}:${line})`)
            } else {
                acc.push(`    at ${file}:${line}`)
            }
            return acc
        }
        return acc
    }, []).join(EOL)
}

function printError(error, stack) {
    const message = error.message + '\n' + makeTrace(stack)
    if (VeSysInfo.debug.fulltrace) {
        error.message = message
        console.error(error)
    } else {
        console.error('Error: ' + message)
    }
}

export {
    fmtError,
    catchError,
    printError,
}

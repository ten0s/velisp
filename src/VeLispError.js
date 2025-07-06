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
import VeSysInfo from './VeSysInfo.js'
import {KFun} from './VeLispTypes.js'
import {EXIT_FAILURE, KERNEL} from './VeConst.js'

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
    return stack.fold((acc, frame) => {
        if (frame.funName) { // if not top frame
            const name = frame.callerName
            let file = undefined
            let line = undefined
            if (name && stack.top().getSym(name.toUpperCase()) instanceof KFun) {
                file = KERNEL
            } else {
                file = path.relative(cwd, frame.callerFile)
                line = frame.callerLine
            }
            if (name && file && line) {
                acc.push(`    at ${name} (${file}:${line})`)
            } else if (name && file) {
                acc.push(`    at ${name} (${file})`)
            } else if (file && line) {
                acc.push(`    at ${file}:${line}`)
            }
            return acc
        }
        return acc
    }, []).join('\n')
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

function printErrorAndExit(error, stack) {
    printError(error, stack)
    process.exit(EXIT_FAILURE)
}

export {
    fmtError,
    catchError,
    printError,
    printErrorAndExit,
}

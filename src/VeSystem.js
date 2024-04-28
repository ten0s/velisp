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
import os from 'os'

const homeDir = () => {
    // MSYS2 or MinGW shell
    if (process.env['MSYSTEM']) {
        return process.env['HOME']
    }
    // Under WSL
    if (process.env['WSLENV']) {
        // Expected that called with at least
        // set WSLENV=TEMP/pu:TMP/pu:USERNAME/u:USERPROFILE/pu
        return process.env['USERPROFILE']
    }
    return os.homedir()
}

const tmpDir = () => {
    const tmp = process.env['TMP']
    if (tmp && fs.existsSync(tmp)) { return tmp }
    const temp = process.env['TEMP']
    if (temp && fs.existsSync(temp)) { return temp }
    return os.tmpdir()
}

const sleep = (msecs) => {
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Atomics/wait
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, msecs)
}

export {
    homeDir,
    tmpDir,
    sleep,
}

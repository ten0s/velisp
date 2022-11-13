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

import {readFileSync} from 'fs'
import {join} from 'path'
import __rootdir from './VeRootDir.js'

const config = JSON.parse(readFileSync(join(__rootdir, 'package.json')))

const platform = () => {
    const os = process.platform
    switch (os) {
    case 'android':
        return 'Android'
    case 'darwin':
        return 'MacOS'
    case 'linux':
        return 'Linux'
    case 'win32':
        return 'Windows'
    default:
        return os
    }
}

const VeSysInfo = {
    prompt: '_$ ',
    isRepl: false,
    withDcl: true,

    name: config.name,
    version: config.version,
    platform: platform(),

    debug: {
        glade: false,
        stacktrace: false,
        tree: false,
        help: false,
    },
}

export default VeSysInfo

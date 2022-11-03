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

import fs from 'fs'
import path from 'path'
import temp from 'temp'
import {Bool, Str, Fun, ensureType} from '../VeLispTypes.js'
import {tmpDir} from '../VeSystem.js'

export const initContext = (context) => {
    context.setSym('VL-FILENAME-MKTEMP', new Fun('vl-filename-mktemp', ['pattern', 'directory', 'extention'], [], (self, args) => {
        if (args.length > 3) {
            throw new Error('vl-filename-mktemp: too many arguments')
        }

        const DEFAULT_NAME = 'velisp-'
        const DEFAULT_DIR  = tmpDir()
        const DEFAULT_EXT  = ''

        let dir  = null
        let name = null
        let ext  = null

        // NB: T behaves like nil to not pollute implementation

        if (args.length === 1) {
            let pattern = ensureType('vl-filename-mktemp: `pattern`', args[0], [Str, Bool])
            if (pattern instanceof Str) {
                ({dir, name, ext} = path.parse(pattern.value()))
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
        }
        if (args.length === 2) {
            let pattern   = ensureType('vl-filename-mktemp: `pattern`'  , args[0], [Str, Bool])
            let directory = ensureType('vl-filename-mktemp: `directory`', args[1], [Str, Bool])
            if (pattern instanceof Str) {
                ({dir, name, ext} = path.parse(pattern.value()))
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
            if (directory instanceof Str) {
                dir = directory.value()
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
        }
        if (args.length === 3) {
            let pattern   = ensureType('vl-filename-mktemp: `pattern`'  , args[0], [Str, Bool])
            let directory = ensureType('vl-filename-mktemp: `directory`', args[1], [Str, Bool])
            let extension = ensureType('vl-filename-mktemp: `extension`', args[2], [Str, Bool])
            if (pattern instanceof Str) {
                ({dir, name, ext} = path.parse(pattern.value()))
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
            if (directory instanceof Str) {
                dir = directory.value()
                if (!fs.existsSync(dir)) {
                    dir = null
                }
            }
            if (extension instanceof Str) {
                ext = extension.value()
            }
        }

        const {path: tmpPath, fd: tmpFd} = temp.track().openSync({
            dir   : dir  ? dir  : DEFAULT_DIR,
            prefix: name ? name : DEFAULT_NAME,
            suffix: ext  ? ext  : DEFAULT_EXT,
        })
        fs.closeSync(tmpFd)

        return new Str(tmpPath)
    }))
}

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

import fs from 'fs'
import path from 'path'

import {Str, Sym, Fun, KFun} from '../VeLispTypes.js'
import {evaluate} from '../VeLispEvaluator.js'
import {fmtError} from '../VeLispError.js'
import VeSysInfo from '../VeSysInfo.js'
import {ensureLspExt, makeUnixPath} from '../VeUtil.js'

export const initContext = (context) => {
    context.setSym('LOAD', new KFun('load', ['filename', '[onfailure]'], [], (self, args) => {
        //console.log('load args:', args)
        if (args.length === 0) {
            throw new Error('load: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('load: too many arguments')
        }
        if (!(args[0] instanceof Str)) {
            throw new Error('load: `filename` expected Str')
        }
        const topContext = self.stack.top()
        const callerFile = topContext.callerFile

        let filename = ensureLspExt(makeUnixPath(args[0].value()))
        if (!path.isAbsolute(filename)) {
            if (!fs.existsSync(filename)) {
                filename = path.join(path.dirname(callerFile), filename)
            }
        }
        try {
            const data = fs.readFileSync(filename).toString()

            // Set temporarily the call file to be the loading file
            // to make it possible to load internal files with
            // relative paths.
            topContext.callerFile = path.resolve(filename)

            const result = evaluate(data, self.stack)

            // Restore back caller file.
            topContext.callerFile = callerFile

            return result
        } catch (e) {
            // Restore back calle file in case of exception!
            topContext.callerFile = callerFile

            if (args.length === 2) {
                let onfailure = args[1]
                if (onfailure instanceof Sym) {
                    // Try resolving symbol to function
                    onfailure = self.stack.top().getSym(onfailure.value())
                }
                if (onfailure instanceof Fun) {
                    return onfailure.apply(self, [])
                }
                return args[1]
            }
            e.path = filename
            throw new Error(fmtError('load', e))
        }
    }))
    context.setSym('VER', new KFun('ver', [], [], (_self, _args) => {
        return new Str(`${VeSysInfo.name} ${VeSysInfo.version} on ${VeSysInfo.platform}`)
    }))
}

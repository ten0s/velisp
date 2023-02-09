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

import {Bool, Int, Str, List, KFun, ensureType} from '../VeLispTypes.js'
import VeStack from '../VeStack.js'
import VeDclContext from '../VeDclContext.js'
import * as VeDclLoader from '../VeDclLoader.js'
import {ListOperation} from '../VeDclTiles.js'
import {
    ensureDclExt,
    ensureSldExt,
    ensureSlbExt,
    makeUnixPath
} from '../VeUtil.js'

// global dclId index
let _dclId = 0
const _dclFiles = {}

const _dialogs = new VeStack()
const _lists   = new VeStack()
const _images  = new VeStack()

const DONE_DIALOG_DELAY = 10

const withDialog = (ifFunc, elseFunc = null) => {
    if (_dialogs.size() === 1) {
        const self = _dialogs.top()
        const parent = null
        return ifFunc(self, parent)
    }
    if (_dialogs.size() >= 2) {
        const self = _dialogs.pop()
        const parent = _dialogs.top()
        _dialogs.push(self)
        return ifFunc(self, parent)
    }
    if (elseFunc) {
        return elseFunc()
    }
    throw new Error('No current dialog')
}

const withList = (ifFunc, elseFunc = null) => {
    if (!_lists.isEmpty()) {
        return ifFunc(_lists.top())
    }
    if (elseFunc) {
        return elseFunc()
    }
    throw new Error('No current list')
}

const withImage = (ifFunc, elseFunc = null) => {
    if (!_images.isEmpty()) {
        return ifFunc(_images.top())
    }
    if (elseFunc) {
        return elseFunc()
    }
    throw new Error('No current image')
}

// :: (string) -> [string, string | undefined]
const parseSlideName = (sldname) => {
    sldname = sldname.trim()

    // Check it's a slide library or slide
    const i = sldname.indexOf('(')
    const j = sldname.indexOf(')')

    if (i >=0 && j == sldname.length-1) {
        // It's a slide library
        return [ensureSlbExt(sldname.substr(0, i)), sldname.substr(i+1, j-i-i)]
    } else if (i == -1 && j == -1) {
        // It's a slide
        return [ensureSldExt(sldname), undefined]
    } else {
        throw new Error('slide_image: `sldname` bad format')
    }
}

export const initContext = (context) => {
    context.setSym('LOAD_DIALOG', new KFun('load_dialog', ['dclfile'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('load_dialog: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('load_dialog: too many arguments')
        }
        let dclFile = ensureDclExt(makeUnixPath(
            ensureType('load_dialog:', args[0], [Str]).value()))
        if (!path.isAbsolute(dclFile)) {
            if (!fs.existsSync(dclFile)) {
                const lspFile = self.stack.top().callerFile
                if (lspFile) {
                    dclFile = path.join(path.dirname(lspFile), dclFile)
                }
            }
        }
        // Return a positive integer, or a negative integer on error
        const dclContext = new VeDclContext()

        // Inject lib/dcl/{base,acad}.dcl
        const rootdir = process.env['VELISP_ROOT']
        VeDclLoader.load(`${rootdir}/lib/dcl/base.dcl`, dclContext)
        VeDclLoader.load(`${rootdir}/lib/dcl/acad.dcl`, dclContext)

        const dclDialogs = VeDclLoader.load(dclFile, dclContext)
        const dclMap = {}
        for (const dclDialog of dclDialogs) {
            dclMap[dclDialog.id] = dclDialog
        }
        const dclId = new Int(_dclId++)
        _dclFiles[dclId.value()] = dclMap
        return dclId
    }))
    context.setSym('NEW_DIALOG', new KFun('new_dialog', ['dlg_id', 'dcl_id', '[action]', '[point]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('new_dialog: too few arguments')
        }
        if (args.length > 4) {
            throw new Error('new_dialog: too many arguments')
        }
        const dlgId = ensureType('new_dialog: `dlg_id`', args[0], [Str])
        const dclId = ensureType('new_dialog: `dcl_id`', args[1], [Int])
        let action = new Str('')
        let point = new List([new Int(-1), new Int(-1)])
        if (args.length > 2) {
            action = ensureType('new_dialog: `action`', args[2], [Str])
        }
        if (args.length > 3) {
            point = ensureType('new_dialog: `point`', args[3], [List])
        }
        const dclFile = _dclFiles[dclId.value()]
        if (dclFile) {
            const dclDialog = dclFile[dlgId.value()]
            if (dclDialog) {
                try {
                    _dialogs.push(dclDialog.clone())
                    return withDialog(dialog => {
                        const position = [point.value()[0].value(), point.value()[1].value()]
                        dialog.gtkInitWidget(action.value(), position, self.stack)
                        return new Bool(true)
                    })
                } catch (e) {
                    // Should never happen since dialog ID is mandatory
                    console.error(e)
                }
            } else {
                throw new Error(`${dlgId} not found in ${dclFile}`)
            }
        } else {
            throw new Error(`${dclId} not found`)
        }
        return new Bool(false)
    }))
    context.setSym('START_DIALOG', new KFun('start_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('start_dialog: too many arguments')
        }
        return withDialog((dialog, parent) => {
            const status = dialog.startDialog(parent)
            return new Int(status)
        })
    }))
    context.setSym('DONE_DIALOG', new KFun('done_dialog', ['[status]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('done_dialog: too many arguments')
        }
        let status = new Int(0)
        if (args.length == 1) {
            status = ensureType('done_dialog:', args[0], [Int])
        }
        return withDialog(_dialog => {
            const [x, y] = _dialogs.pop().doneDialog(status.value())
            return new List([new Int(x), new Int(y)])
        })
    }))
    context.setSym('TERM_DIALOG', new KFun('term_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('term_dialog: too many arguments')
        }
        let i = 0
        while (!_dialogs.isEmpty()) {
            const dialog = _dialogs.pop()
            setTimeout(() => dialog.doneDialog(0), DONE_DIALOG_DELAY * i++)
        }
        return new Bool(false)
    }))
    context.setSym('UNLOAD_DIALOG', new KFun('unload_dialog', ['dcl_id'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('unload_dialog: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('unload_dialog: too many arguments')
        }
        const dclId = ensureType('unload_dialog:', args[0], [Int])
        delete _dclFiles[dclId.value()]
        return new Bool(false)
    }))
    context.setSym('ACTION_TILE', new KFun('action_tile', ['key', 'action'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('action_tile: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('action_tile: too many arguments')
        }
        const key = ensureType('action_tile: `key`', args[0], [Str])
        const action = ensureType('action_tile: `action`', args[1], [Str])
        //console.log(action.toUnescapedString());
        return withDialog(dialog => {
            try {
                dialog.actionTile(key.value(), action.value(), self.stack)
                return new Bool(true)
            } catch (e) {
                console.error(e)
                return new Bool(false)
            }
        })
    }))
    context.setSym('CLIENT_DATA_TILE', new KFun('client_data_tile', ['key', 'data'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('client_data_tile: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('client_data_tile: too many arguments')
        }
        const key = ensureType('client_data_tile: `key`', args[0], [Str])
        const data = ensureType('client_data_tile: `data`', args[1], [Str])
        return withDialog(dialog => {
            dialog.clientDataTile(key.value(), data.value())
            return new Bool(false)
        })
    }))
    context.setSym('GET_ATTR', new KFun('get_attr', ['key', 'attr'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('get_attr: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('get_attr: too many arguments')
        }
        const key = ensureType('get_attr:', args[0], [Str])
        const attr = ensureType('get_attr:', args[1], [Str])
        return withDialog(dialog => {
            const str = dialog.getAttr(key.value(), attr.value())
            return new Str(str)
        })
    }))
    context.setSym('GET_TILE', new KFun('get_tile', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('get_tile: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('get_tile: too many arguments')
        }
        const key = ensureType('get_tile:', args[0], [Str])
        return withDialog(dialog => {
            const str = dialog.getTile(key.value())
            return new Str(str)
        })
    }))
    context.setSym('SET_TILE', new KFun('get_tile', ['key', 'value'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('set_tile: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('set_tile: too many arguments')
        }
        const key = ensureType('set_tile:', args[0], [Str])
        const value = ensureType('set_tile:', args[1], [Str])
        return withDialog(dialog => {
            dialog.setTile(key.value(), value.value())
            return value
        })
    }))
    context.setSym('MODE_TILE', new KFun('mode_tile', ['key', 'mode'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('mode_tile: too few arguments')
        }
        if (args.length > 2) {
            throw new Error('mode_tile: too many arguments')
        }
        const key = ensureType('mode_tile: `key`', args[0], [Str])
        const mode = ensureType('mode_tile: `mode`', args[1], [Int])
        return withDialog(dialog => {
            dialog.modeTile(key.value(), mode.value())
            return new Bool(false)
        })
    }))
    context.setSym('START_LIST', new KFun('start_list', ['key', '[operation]', '[index]'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('start_list: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('start_list: too many arguments')
        }
        const key = ensureType('start_list: `key`', args[0], [Str])
        let operation
        if (args.length > 1) {
            operation = ensureType('start_list: `operation`', args[1], [Int])
        } else {
            operation = new Int(ListOperation.CLEAR)
        }
        let index
        if (args.length > 2) {
            index = ensureType('start_list: `index`', args[2], [Int])
        } else {
            // Used for ListOperation.CHANGE only
            index = new Int(0)
        }
        return withDialog(dialog => {
            _lists.push(dialog.startList(key.value(), operation.value(), index.value()))
            return key
        })
    }))
    context.setSym('ADD_LIST', new KFun('add_list', ['str'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('add_list: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('add_list: too many arguments')
        }
        const str = ensureType('add_list: `str`', args[0], [Str])
        return withDialog(dialog => {
            withList(list => {
                dialog.addList(list, str.value())
                return str
            })
        })
    }))
    context.setSym('END_LIST', new KFun('end_list', [], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('end_list: too many arguments')
        }
        return withDialog(dialog => {
            return withList(_list => {
                dialog.endList(_lists.pop())
                return new Bool(false)
            })
        })
    }))
    context.setSym('DIMX_TILE', new KFun('dimx_tile', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('dimx_tile: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('dimx_tile: too many arguments')
        }
        const key = ensureType('dimx_tile: `key`', args[0], [Str])
        return withDialog(dialog => {
            const dimX = dialog.dimXTile(key.value())
            return new Int(dimX)
        })
    }))
    context.setSym('DIMY_TILE', new KFun('dimy_tile', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('dimy_tile: too few arguments')
        }
        if (args.length > 1) {
            throw new Error('dimy_tile: too many arguments')
        }
        const key = ensureType('dimy_tile: `key`', args[0], [Str])
        return withDialog(dialog => {
            const dimY = dialog.dimYTile(key.value())
            return new Int(dimY)
        })
    }))
    context.setSym('START_IMAGE', new KFun('start_list', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('start_image: too few arguments')
        }
        if (args.length > 3) {
            throw new Error('start_image: too many arguments')
        }
        const key = ensureType('start_image: `key`', args[0], [Str])
        return withDialog(dialog => {
            _images.push(dialog.startImage(key.value()))
            return key
        })
    }))
    context.setSym('FILL_IMAGE', new KFun('fill_image', ['x1', 'y1', 'width', 'height', 'color'], [], (self, args) => {
        if (args.length < 5) {
            throw new Error('fill_image: too few arguments')
        }
        if (args.length > 5) {
            throw new Error('fill_image: too many arguments')
        }
        const x = ensureType('fill_image: `x1`'    , args[0], [Int])
        const y = ensureType('fill_image: `y1`'    , args[1], [Int])
        const w = ensureType('fill_image: `width`' , args[2], [Int])
        const h = ensureType('fill_image: `height`', args[3], [Int])
        const c = ensureType('fill_image: `color`' , args[4], [Int])
        return withDialog(dialog => {
            return withImage(image => {
                dialog.fillImage(
                    image, x.value(), y.value(), w.value(), h.value(), c.value()
                )
                return c
            })
        })
    }))
    context.setSym('VECTOR_IMAGE', new KFun('vector_image', ['x1', 'y1', 'x2', 'y2', 'color'], [], (self, args) => {
        if (args.length < 5) {
            throw new Error('vector_image: too few arguments')
        }
        if (args.length > 5) {
            throw new Error('vector_image: too many arguments')
        }
        const x1 = ensureType('vector_image: `x1`'   , args[0], [Int])
        const y1 = ensureType('vector_image: `y1`'   , args[1], [Int])
        const x2 = ensureType('vector_image: `x2`'   , args[2], [Int])
        const y2 = ensureType('vector_image: `y2`'   , args[3], [Int])
        const c  = ensureType('vector_image: `color`', args[4], [Int])
        return withDialog(dialog => {
            return withImage(image => {
                dialog.vectorImage(
                    image, x1.value(), y1.value(), x2.value(), y2.value(), c.value()
                )
                return c
            })
        })
    }))
    context.setSym('SLIDE_IMAGE', new KFun('slide_image', ['x', 'y', 'width', 'height', 'sldname'], [], (self, args) => {
        if (args.length < 5) {
            throw new Error('slide_image: too few arguments')
        }
        if (args.length > 5) {
            throw new Error('slide_image: too many arguments')
        }
        const x = ensureType('slide_image: `x`'      , args[0], [Int]).value()
        const y = ensureType('slide_image: `y`'      , args[1], [Int]).value()
        const w = ensureType('slide_image: `width`'  , args[2], [Int]).value()
        const h = ensureType('slide_image: `height`' , args[3], [Int]).value()
        const sldName = ensureType('slide_image: `sldname`', args[4], [Str]).value()

        let [sldFile, name] = parseSlideName(makeUnixPath(sldName))
        if (!path.isAbsolute(sldFile)) {
            if (!fs.existsSync(sldFile)) {
                const lspFile = self.stack.top().callerFile
                if (lspFile) {
                    sldFile = path.join(path.dirname(lspFile), sldFile)
                }
            }
        }
        const sldUri = name ? `${sldFile}(${name})` : sldFile
        return withDialog(dialog => {
            return withImage(image => {
                dialog.slideImage(
                    image, x, y, w, h, sldFile
                )
                return sldName
            })
        })
    }))
    context.setSym('END_IMAGE', new KFun('end_image', [], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('end_image: too many arguments')
        }
        return withDialog(dialog => {
            return withImage(_image => {
                dialog.endImage(_images.pop())
                return new Bool(false)
            })
        })
    }))
}

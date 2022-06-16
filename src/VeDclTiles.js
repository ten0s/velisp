import {Int, Str} from './VeLispTypes.js'
import RGB from './VeDclRGB.js'
import {unescape} from './VeUtil.js'
import * as VeLispEvaluator from './VeLispEvaluator.js'

import gi from 'node-gtk'

let Gtk = null
let Gdk = null
let GObject = null

const InitGtk = () => {
    if (!Gtk || !Gdk || !GObject) {
        Gtk = gi.require('Gtk', '3.0')
        Gdk = gi.require('Gdk', '3.0')
        // Required under X11?
        //Gdk = gi.require('GdkX11', '3.0')
        GObject = gi.require('GObject')

        gi.startLoop()
        Gtk.init()
        Gdk.init([])
    }
}
// TODO: Lazy load from Tile constructor
// examples/dcl/run_all.sh doesn't work
InitGtk()

const TileMode = {
    ENABLE_TILE: 0,
    DISABLE_TILE: 1,
    FOCUS_TILE: 2,
    SELECT_EDITBOX: 3,
    FLIP_IMAGE_HIGHLIGHT: 4,
}

const Layout = {
    COLUMN: 'column',
    ROW: 'row',
}

const Alignment = {
    LEFT: 'left',
    RIGHT: 'right',
    TOP: 'top',
    BOTTOM: 'bottom',
    CENTERED: 'centered',
    FILLED: 'filled', // GTK specific
}

// GTK specific
const Justify = {
    LEFT: 'left',
    RIGHT: 'right',
}

const ListOperation = {
    CHANGE: 1,
    APPEND: 2,
    CLEAR: 3,
}

const ActionReason = {
    TILE_SELECTED: 1,
    EDIT_BOX_LOST_FOCUS: 2,
    SLIDER_INTERIM_CHANGE: 3,
    TILE_DOUBLE_CLICKED: 4,
}

const PARENT_DX = 60
const PARENT_DY = -20

class Tile {
    constructor(id) {
        // Attributes
        this.id = id
        this.height = -1
        this.width = -1
        // Locals (must be explicitly re-assigned in `clone')
        this._clientData = ''
        this._drawOperations = []
    }

    addAttribute(name, value) {
        if (this.hasOwnProperty(name)) {
            this[name] = value
            return true
        }
        return false
    }

    clone() {
        const clone = Object.assign(Object.create(Object.getPrototypeOf(this)), this)
        clone._clientData = ''
        clone._drawOperations = []
        return clone
    }

    _child(xml) {
        return `<child>${xml}</child>`
    }

    _bool(value) {
        return value ? 'True' : 'False'
    }

    _bold(value) {
        return value ? 'bold' : 'normal'
    }

    _dcl_to_gtk_align(alignment) {
        switch (alignment) {
        case Alignment.LEFT:
            return 'start'
        case Alignment.RIGHT:
            return 'end'
        case Alignment.TOP:
            return 'start'
        case Alignment.BOTTOM:
            return 'end'
        case Alignment.CENTERED:
            return 'center'
        case Alignment.FILLED:
            return 'fill'
        default:
            throw new Error(`Invalid alignment: ${alignment}`)
        }
    }

    _xalign(justify) {
        switch (justify) {
        case Justify.LEFT:
            return 0
        case Justify.RIGHT:
            return 1
        default:
            console.error(`Error: Invalid justify '${justify}'. Defaulting to '${Justify.LEFT}'`)
            return this._xalign(Justify.LEFT)
        }
    }

    _halign(alignment, layout) {
        switch (layout) {
        case Layout.COLUMN:
            return this._dcl_to_gtk_align(alignment ? alignment : Alignment.LEFT)
        case Layout.ROW:
            return 'fill'
        default:
            throw new Error(`Invalid layout: ${layout}`)
        }
    }

    _valign(alignment, layout) {
        switch (layout) {
        case Layout.COLUMN:
            return 'fill'
        case Layout.ROW:
            return this._dcl_to_gtk_align(alignment ? alignment : Alignment.CENTERED)
        default:
            throw new Error(`Invalid layout: ${layout}`)
        }
    }

    _width(value) {
        if (value === -1) {
            return value
        }
        // TODO: Calculate based on font
        // https://help.autodesk.com/view/OARX/2019/ENU/?guid=GUID-F5ACFAA1-4528-4BC5-B45E-0F7C114AFEDE
        return 7.5 * value
    }

    _height(value) {
        if (value === -1) {
            return value
        }
        // TODO: Calculate based on font
        // https://help.autodesk.com/view/OARX/2019/ENU/?guid=GUID-DD0E03A8-D2EB-4D79-9BF7-D49EFD3820D4
        return 15 * value
    }

    // * if there's '&' in label, return label with '&' replaced by '_'
    // * if there's a non-empty mnemonic try to merge it into label, return '' otherwise
    // * return '' otherwise
    _mnemonicLabel(mnemonic, label = '') {
        if (label.includes('&')) {
            return label.split('&').join('_')
        } else if (mnemonic.trim()) {
            let output = ''
            let merged = false
            mnemonic = mnemonic.trim()[0].toLowerCase()
            for (let i = 0; i < label.length; i++) {
                const char = label.charAt(i)
                if (!merged && char.toLowerCase() === mnemonic) {
                    output += '_'
                    merged = true
                }
                output += char
            }
            if (merged) {
                return output
            }
        }
        return ''
    }

    // * if there's '&' in label, return char following '&', if one
    // * if there's a non-empty mnemonic, return it's first char
    // * return '' otherwise
    _mnemonicChar(mnemonic, label = '') {
        const index = label.indexOf('&')
        if (index !== -1 && index + 1 < label.length) {
            return label.charAt(index + 1)
        } else if (mnemonic.trim()) {
            return mnemonic.trim()[0].toLowerCase()
        }
        return ''
    }

    _escape(text) {
        return text.split('&').join('&amp;')
    }

    // Optional
    gtkInitWidget(_gtkWidget) { }

    gtkActionTile(_gtkWidget, _action, _context) {
        throw new Error(
            `Not implemented gtkActionTile for ${this.constructor.name}`
        )
    }

    gtkGetTile(_gtkWidget) {
        throw new Error(
            `Not implemented gtkGetTile for ${this.constructor.name}`
        )
    }

    gtkSetTile(_gtkWidget, _value) {
        throw new Error(
            `Not implemented gtkSetTile for ${this.constructor.name}`
        )
    }

    gtkDimX(_gtkWidget) {
        return Math.round(this._width(this.width))
    }

    gtkDimY(_gtkWidget) {
        return Math.round(this._height(this.height))
    }

    gtkAppendDrawOperations(operations) {
        operations.forEach(op => this._drawOperations.push(op))
    }

    gtkDraw(gtkWidget, gtkCtx) {
        this._drawOperations.forEach(op => op.gtkDraw(gtkWidget, gtkCtx))
    }

    gtkXml() {
        throw new Error(
            'Not implemented gtkXml for ${this.constructor.name}'
        )
    }
}

class FillImage {
    constructor(x, y, w, h, c) {
        this.x = x
        this.y = y
        this.w = w
        this.h = h
        this.c = c
    }

    gtkDraw(gtkWidget, gtkCtx) {
        const rgb = RGB.fromACI(this.c)
        gtkCtx.setSourceRgb(rgb.r, rgb.g, rgb.b)
        gtkCtx.rectangle(this.x, this.y, this.w, this.h)
        gtkCtx.fill()
    }
}

class VectorImage {
    constructor(x1, y1, x2, y2, c) {
        this.x1 = x1
        this.y1 = y1
        this.x2 = x2
        this.y2 = y2
        this.c = c
    }

    gtkDraw(gtkWidget, gtkCtx) {
        const rgb = RGB.fromACI(this.c)
        gtkCtx.setSourceRgb(rgb.r, rgb.g, rgb.b)
        gtkCtx.setLineWidth(1)
        gtkCtx.moveTo(this.x1, this.y1)
        gtkCtx.lineTo(this.x2, this.y2)
        gtkCtx.stroke()
    }
}

class Cluster extends Tile {
    constructor(id) {
        super(id)
        // Locals
        this._tiles = []
    }

    addTile(tile) {
        this._tiles.push(tile)
    }

    getKeys() {
        let list = []
        for (let tile of this._tiles) {
            if (tile instanceof Cluster) {
                list = list.concat(tile.getKeys())
            } else {
                if (tile.key) {
                    list.push(tile.key)
                }
            }
        }
        return list
    }

    getActions() {
        let list = []
        for (let tile of this._tiles) {
            if (tile instanceof Cluster) {
                list = list.concat(tile.getActions())
            } else {
                if (tile.action && tile.key) {
                    list.push([tile.key, tile.action])
                }
            }
        }
        return list
    }

    getListStores() {
        let list = []
        for (let tile of this._tiles) {
            if (tile instanceof Cluster) {
                list = list.concat(tile.getListStores())
            } else {
                if (tile.hasOwnProperty('list') && tile.key) {
                    list.push([tile.key, tile.list, tile.tabs])
                }
            }
        }
        return list
    }

    findTile(key) {
        if (this.key === key) {
            return this
        }
        for (let tile of this._tiles) {
            if (tile.key === key) {
                return tile
            }
            if (tile instanceof Cluster) {
                const found = tile.findTile(key)
                if (found) {
                    return found
                }
            }
        }
        return null
    }
}

class Dialog extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.initial_focus = null
        this.is_resizable = false // GTK specific
        this.height = -1
        this.key = null
        this.label = ''
        this.value = ''
        this.width = -1
        // Locals
        delete this._tiles
        this._column = new Column()
        this._gtkBuilder = null
        this._gtkWindow = null
        this._initPosition = null // [ x, y ]
        this._listStores = null   // [ [key, list, tabs] ]
        this._status = null
    }

    // Cluster
    addTile(tile) {
        this._column.addTile(tile)
    }

    // Cluster
    getKeys() {
        return this._column.getKeys()
    }

    // Cluster
    getActions() {
        return this._column.getActions()
    }

    // Cluster
    getListStores() {
        return this._column.getListStores()
    }

    // Cluster
    findTile(key) {
        if (this.key === key) {
            return this
        }
        const tile = this._column.findTile(key)
        if (tile) {
            return tile
        }
        throw new Error(`Tile not found: ${key}`)
    }

    // DCL
    actionTile(key, action, context) {
        const tile = this.findTile(key)
        const gtkWidget = this.gtkFindWidget(key)
        tile.gtkActionTile(gtkWidget, action, context)
    }

    // DCL
    clientDataTile(key, data) {
        const tile = this.findTile(key)
        tile._clientData = data
    }

    // DCL
    getAttr(key, attr) {
        const tile = this.findTile(key)
        if (tile.hasOwnProperty(attr)) {
            return tile[attr].toString()
        }
        return ''
    }

    // DCL
    getTile(key) {
        if (this.key === key) {
            return this.gtkGetTile(this._gtkWindow)
        }
        const tile = this.findTile(key)
        const gtkWidget = this.gtkFindWidget(key)
        return tile.gtkGetTile(gtkWidget)
    }

    // DCL
    setTile(key, value) {
        if (this.key === key) {
            return this.gtkSetTile(this._gtkWindow, value)
        }
        const tile = this.findTile(key)
        const gtkWidget = this.gtkFindWidget(key)
        return tile.gtkSetTile(gtkWidget, value)
    }

    // DCL
    modeTile(key, mode) {
        const gtkWidget = this.gtkFindWidget(key)
        switch (mode) {
        case TileMode.ENABLE_TILE:
            gtkWidget.setSensitive(true)
            break
        case TileMode.DISABLE_TILE:
            gtkWidget.setSensitive(false)
            break
        case TileMode.FOCUS_TILE:
            this._gtkWindow.setFocus(gtkWidget)
            break
        case TileMode.FLIP_IMAGE_HIGHLIGHT:
            console.error(`Error: not implemented tile mode '${mode}'`)
            break
        default:
            console.error(`Error: unknown tile mode '${mode}'`)
        }
    }

    // DCL
    startDialog(parent) {
        this._gtkWindow.setModal(true)
        this._gtkWindow.setResizable(this.is_resizable)
        // TODO: calculate using both length and font
        //console.log(this._gtkWindow.getTitle().length)
        const fixMeWidth = this._gtkWindow.getTitle().length * 8.4 + 160
        const width = Math.max(this._width(this.width), fixMeWidth)
        const height = this._height(this.height)
        this._gtkWindow.setSizeRequest(width, height)
        if (this._initPosition[0] >= 0 && this._initPosition[1] >= 0) {
            this._gtkWindow.move(
                this._initPosition[0],
                this._initPosition[1]
            )
        } else if (parent) {
            const parentPos = parent._gtkWindow.getPosition()
            this._gtkWindow.move(
                parentPos[0] + PARENT_DX,
                parentPos[1] + PARENT_DY
            )
        }
        this._gtkWindow.on('show', Gtk.main)
        this._gtkWindow.on('destroy', Gtk.mainQuit)
        // Blocking call, this._gtkWindow is null after it! See doneDialog.
        this._gtkWindow.showAll()
        // See doneDialog for status.
        const status = this._status ? this._status : 0
        return status
    }

    // DCL
    doneDialog(status) {
        // See startDialog for status
        this._status = status
        const pos = this._gtkWindow.getPosition()
        this._gtkWindow.destroy()
        this._gtkWindow = null
        this._gtkBuilder = null
        return pos
    }

    // DCL
    startList(key, operation, index) {
        /*const tile = */this.findTile(key)
        const gtkWidget = this.gtkFindWidget(key)
        const listStore = gtkWidget.getModel()
        switch (operation) {
        case ListOperation.CHANGE:
            return {
                operation,
                index,
                listStore,
            }
        case ListOperation.APPEND:
            return {
                operation,
                listStore,
            }
        case ListOperation.CLEAR:
            this.gtkListClear(listStore)
            return {
                operation: ListOperation.APPEND,
                listStore,
            }
        default:
            throw new Error(`Unknown start_list operation: ${operation}`)
        }
    }

    // DCL
    addList({operation, index, listStore}, str) {
        switch (operation) {
        case ListOperation.CHANGE:
            this.gtkListChange(listStore, index, str)
            break
        case ListOperation.APPEND:
            this.gtkListAppend(listStore, str)
            break
        default:
            throw new Error(`Unknown add_list operation: ${operation}`)
        }
    }

    gtkListClear(listStore) {
        listStore.clear()
    }

    gtkListChange(listStore, index, str) {
        const path = new Gtk.TreePath.newFromIndices([index])
        const [valid, iter] = listStore.getIter(path)
        if (!valid) {
            throw new Error(`Invalid add_list index: ${index}`)
        }
        this.gtkListSet(listStore, iter, str)
    }

    gtkListAppend(listStore, str) {
        const iter = listStore.append()
        this.gtkListSet(listStore, iter, str)
    }

    gtkListSet(listStore, iter, str) {
        const value = new GObject.Value()
        value.init(GObject.TYPE_STRING)
        value.setString(str)
        listStore.setValue(iter, 0, value)
    }

    // DCL
    endList(_) {}

    // DCL
    dimXTile(key) {
        const tile = this.findTile(key)
        const gtkWidget = this.gtkFindWidget(key)
        return tile.gtkDimX(gtkWidget)
    }

    // DCL
    dimYTile(key) {
        const tile = this.findTile(key)
        const gtkWidget = this.gtkFindWidget(key)
        return tile.gtkDimY(gtkWidget)
    }

    // DCL
    startImage(key) {
        const tile = this.findTile(key)
        /*const gtkWidget = */this.gtkFindWidget(key)
        return {
            tile,
            operations: [],
        }
    }

    // DCL
    fillImage(handle, x, y, w, h, color) {
        handle.operations.push(new FillImage(x, y, w, h, color))
    }

    // DCL
    vectorImage(handle, x1, y1, x2, y2, color) {
        handle.operations.push(new VectorImage(x1, y1, x2, y2, color))
    }

    // DCL
    endImage({tile, operations}) {
        tile.gtkAppendDrawOperations(operations)
        // Kick redraw
        const gtkWidget = this.gtkFindWidget(tile.key)
        gtkWidget.queueDraw()
    }

    // GTK
    gtkInitWidget(defaultAction, initPosition, context) {
        // Pre init
        this._initPosition = initPosition
        this._listStores = this.getListStores()
        // Init
        const gtkXml = this.gtkXml()
        //console.log(gtkXml)
        this._gtkBuilder = new Gtk.Builder()
        this._gtkBuilder.addFromString(gtkXml, gtkXml.length)
        this._gtkWindow = this.gtkFindWidget(this.id)
        // Post init:
        // 1. Init widgets
        for (let key of this.getKeys()) {
            const tile = this.findTile(key)
            const gtkWidget = this.gtkFindWidget(key)
            // Init widget
            tile.gtkInitWidget(gtkWidget)
            // Optional dialog action
            if (defaultAction) {
                if (tile.hasOwnProperty('action') && !tile.action) {
                    this.actionTile(key, defaultAction, context)
                }
            }
        }
        // 2. Init attribute actions
        for (let [key, action] of this.getActions()) {
            this.actionTile(key, action, context)
        }
        // 3. Optional initial focus
        if (this.initial_focus) {
            const gtkWidget = this.gtkFindWidget(this.initial_focus)
            this._gtkWindow.setFocus(gtkWidget)
        }
    }

    gtkFindWidget(key) {
        if (this.key === key) {
            return this._gtkWindow
        }
        const gtkWidget = this._gtkBuilder.getObject(key)
        if (gtkWidget) {
            return gtkWidget
        }
        throw new Error(`Widget not found: ${key}`)
    }

    gtkGetTile(gtkWidget) {
        // MUST BE gtkWidget === this._gtkWindow
        return gtkWidget.getTitle()
    }

    gtkSetTile(gtkWidget, value) {
        // MUST BE gtkWidget === this._gtkWindow
        gtkWidget.setTitle(value)
    }

    gtkXml() {
        const id = this.id ? `id="${this.id}"` : ''
        const title = this.label ? this.label : this.value
        const child = this._child(this._column.gtkXml({layout: Layout.COLUMN, is_top: true}))
        const listStores = this._listStores.map(([key, list, tabs]) => {
            return (new  ListStore(key, list, tabs)).gtkXml()
        }).join('\n')
        return `
<?xml version="1.0" encoding="UTF-8"?>
<interface>
  <requires lib="gtk+" version="3.20"/>
  <object class="GtkWindow" ${id}>
    <property name="can_focus">False</property>
    <property name="title">${title}</property>
    ${child}
  </object>
  ${listStores}
</interface>
`
    }
}

class ListStore {
    constructor(key, list, tabs) {
        // Attributes
        this.key = key
        this.list = list
        this.tabs = tabs
    }

    _column() {
        return '<column type="gchararray"/>'
    }

    _row(value) {
        return `<row><col id="0">${value}</col></row>`
    }

    gtkXml() {
        const rows = this.list.split('\\n').map(this._row).join('\n')
        return `
<object class="GtkListStore" id="liststore-${this.key}">
  <columns>
    ${this._column()}
  </columns>
  <data>
    ${rows}
  </data>
</object>
`
    }
}

class Row extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.FILLED
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        //this.label = ''
        this.width = -1
    }

    gtkXml({layout}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW}))
        ).join('\n')
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <!-- Without a border around it -->
  <property name="margin_left">0</property>
  <property name="margin_right">0</property>
  <property name="margin_top">0</property>
  <property name="margin_bottom">0</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <!-- Whether the children should all be the same size? No -->
  <property name="homogeneous">False</property>
  ${tiles}
</object>
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
    }
}

class Column extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.FILLED
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        //this.label = ''
        this.width = -1
    }

    gtkXml({layout, is_top}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN}))
        ).join('\n')
        let packing = ''
        if (!is_top) {
            packing = `
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
        }
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <!-- Without a border around it -->
  <property name="margin_left">0</property>
  <property name="margin_right">0</property>
  <property name="margin_top">0</property>
  <property name="margin_bottom">0</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  ${tiles}
</object>
${packing}
`
    }
}

class BoxedRow extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.FILLED
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.key = null
        this.label = ''
        this.width = -1
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW}))
        ).join('\n')
        return `
<object class="GtkFrame">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <!-- A row with a border around it -->
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="label_xalign">0.03</property>
  <child>
    <object class="GtkBox" ${id}>
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="orientation">horizontal</property>
      <property name="halign">${this._halign(this.alignment, layout)}</property>
      <property name="valign">${this._valign(this.alignment, layout)}</property>
      <property name="width_request">${this._width(this.width)}</property>
      <property name="height_request">${this._height(this.height)}</property>
      ${tiles}
    </object>
  </child>
  <child type="label">
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
    }
}

class BoxedColumn extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.FILLED
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.key = null
        this.label = ''
        this.width = -1
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN}))
        ).join('\n')
        return `
<object class="GtkFrame">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <!-- A column with a border around it -->
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="label_xalign">0.029999999329447746</property>
  <child>
    <object class="GtkBox" ${id}>
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="orientation">vertical</property>
      <property name="halign">${this._halign(this.alignment, layout)}</property>
      <property name="valign">${this._valign(this.alignment, layout)}</property>
      <property name="width_request">${this._width(this.width)}</property>
      <property name="height_request">${this._height(this.height)}</property>
      ${tiles}
    </object>
  </child>
  <child type="label">
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
    }
}

class Concatenation extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.CENTERED
        this.height = -1
        this.width = -1
    }

    gtkXml({layout}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW}))
        ).join('\n')
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <!-- A margin around the concatenation as a whole -->
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <!-- text_part(s) might have diff width(s) -->
  <property name="homogeneous">False</property>
  ${tiles}
</object>
`
    }
}

class Paragraph extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.LEFT
        this.height = -1
        this.width = -1
    }

    gtkXml({layout}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN}))
        ).join('\n')
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <!-- A margin around the paragraph as a whole -->
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  ${tiles}
</object>
`
    }
}

class Spacer extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = ''
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.width = -1
    }

    gtkXml({layout}) {
        return `
<object class="GtkLabel">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label"></property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
</object>
`
    }
}

class Text extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = ''
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.is_bold = false
        this.key = null
        this.label = ''
        this.value = ''
        this.width = -1
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getText()
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value)
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        const label = this.label ? this.label : this.value
        return `
<object class="GtkLabel" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label">${this._escape(label)}</property>
  <!-- Align text left if width is given -->
  <property name="hexpand">True</property>
  <property name="xalign">0.00</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <attributes>
    <attribute name="weight" value="${this._bold(this.is_bold)}"/>
  </attributes>
</object>
`
    }
}

class TextPart extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.LEFT
        this.height = -1
        this.is_bold = false
        this.key = null
        this.label = ''
        this.value = ''
        this.width = -1
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getText()
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value)
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        const label = this.label ? this.label : this.value
        return `
<object class="GtkLabel" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label">${this._escape(label)}</property>
  <property name="margin_left">2</property>
  <property name="margin_right">2</property>
  <property name="margin_top">0</property>
  <property name="margin_bottom">0</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <attributes>
    <attribute name="weight" value="${this._bold(this.is_bold)}"/>
  </attributes>
</object>
<packing>
  <property name="expand">False</property>
  <property name="fill">False</property>
</packing>
`
    }
}

class Button extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        this.alignment = ''
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        //this.is_cancel = false
        this.is_default = false
        this.is_enabled = true
        this.is_tab_stop = true
        this.key = null
        this.label = ''
        this.mnemonic = ''
        this.width = -1
        // Locals
        this._action = null
        this._callback = null
    }

    gtkInitWidget(gtkWidget) {
        this._action = this.action
        const mnemonicLabel = this._mnemonicLabel(this.mnemonic, this.label)
        if (mnemonicLabel) {
            const gtkLabel = gtkWidget.getChildren()[0]
            gtkLabel.setTextWithMnemonic(mnemonicLabel)
            gtkLabel.setMnemonicWidget(gtkWidget)
        }
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        this._callback && gtkWidget.off('clicked', this._callback)
        this._callback = () => {
            context.setVar('$KEY', new Str(this.key))
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)))
            context.setVar('$DATA', new Str(this._clientData))
            context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
            VeLispEvaluator.evaluate(unescape(this._action), context)
        }
        gtkWidget.on('clicked', this._callback)
    }

    gtkGetTile(gtkWidget) {
        const gtkLabel = gtkWidget.getChildren()[0]
        return gtkLabel.getLabel()
    }

    gtkSetTile(gtkWidget, value) {
        const gtkLabel = gtkWidget.getChildren()[0]
        gtkLabel.setLabel(value)
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        return `
<object class="GtkButton" ${id}>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">${this._bool(this.is_tab_stop)}</property>
  <property name="can_default">True</property>
  <property name="has_default">${this._bool(this.is_default)}</property>
  <property name="receives_default">True</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <child>
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`
    }
}

class EditBox extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        this.alignment = ''
        //this.allow_accept = false
        this.edit_limit = 132
        this.edit_width = 0
        this.justify = Justify.LEFT // GTK specific
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.is_enabled = true
        this.is_tab_stop = true
        this.key = null
        this.label = ''
        this.mnemonic = ''
        this.value = ''
        this.width = -1
        //this.password_char = '*'
        // Locals
        this._action = null
        this._callback = null
    }

    gtkInitWidget(gtkWidget) {
        this._action = this.action
        const mnemonicLabel = this._mnemonicLabel(this.mnemonic, this.label)
        if (mnemonicLabel) {
            const gtkLabel = gtkWidget.getParent().getChildren()[0]
            gtkLabel.setTextWithMnemonic(mnemonicLabel)
            gtkLabel.setMnemonicWidget(gtkWidget)
        }
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        this._callback && gtkWidget.off('changed', this._callback)
        this._callback = () => {
            context.setVar('$KEY', new Str(this.key))
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)))
            context.setVar('$DATA', new Str(this._clientData))
            // TODO: Implement reason 2 - Edit Box lost focus?
            context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
            VeLispEvaluator.evaluate(unescape(this._action), context)
        }
        gtkWidget.on('changed', this._callback)
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getText()
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value)
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <!-- Not sure for now how it should align
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  -->
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <child>
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
      <property name="justify">left</property>
      <property name="margin_right">${this.label ? 4 : 0}</property>
    </object>
    <packing>
      <property name="fill">True</property>
      <property name="expand">False</property>
      <property name="pack_type">start</property>
      <property name="position">0</property>
    </packing>
  </child>
  <child>
    <object class="GtkEntry" ${id}>
      <property name="visible">True</property>
      <property name="sensitive">${this._bool(this.is_enabled)}</property>
      <property name="can_focus">${this._bool(this.is_tab_stop)}</property>
      <property name="max_length">${this.edit_limit}</property>
      <property name="width_chars">${this.edit_width}</property>
      <property name="text">${this.value}</property>
      <property name="xalign">${this._xalign(this.justify)}</property>
    </object>
    <packing>
      <property name="fill">True</property>
      <property name="expand">${this._bool(!this.edit_width > 0)}</property>
      <property name="pack_type">end</property>
      <property name="position">1</property>
    </packing>
  </child>
</object>
`
    }
}

class Image extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        //this.action = ''
        this.alignment = ''
        //this.aspect_radio = null
        this.color = ''
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = 10
        this.is_enabled = true
        this.is_tab_stop = true
        this.key = null
        //this.mnemonic = ''
        this.value = ''
        this.width = 10
    }

    gtkInitWidget(gtkWidget) {
        if (this.color) {
            this.gtkAppendDrawOperations([
                new FillImage(0, 0, this._width(this.width), this._height(this.height), this.color)
            ])
        }
        gtkWidget.on('draw', (ctx) => {
            this.gtkDraw(gtkWidget, ctx)
            return false
        })
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <child>
    <object class="GtkDrawingArea" ${id}>
      <property name="visible">True</property>
      <property name="can_focus">${this._bool(this.is_tab_stop)}</property>
      <property name="sensitive">${this._bool(this.is_enabled)}</property>
    </object>
    <packing>
       <property name="expand">True</property>
       <property name="fill">True</property>
    </packing>
  </child>
</object>
`
    }
}

class ImageButton extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        this.alignment = ''
        //this.allow_accept = false
        //this.aspect_radio = null
        this.color = ''
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = 10
        this.is_enabled = true
        this.is_tab_stop = true
        this.key = null
        this.mnemonic = ''
        this.value = ''
        this.width = 10
        // Locals
        this._action = null
        this._callback = null
        this._x = 0
        this._y = 0
        this._reason = ActionReason.TILE_SELECTED
    }

    gtkInitWidget(gtkWidget) {
        this._action = this.action
        gtkWidget.on('button-press-event', (event) => {
            this._reason = ActionReason.TILE_SELECTED
            if (event.type === Gdk.EventType.BUTTON_PRESS ||
                event.type === Gdk.EventType.DOUBLE_BUTTON_PRESS) {
                this._x = Math.round(event.x | 0)
                this._y = Math.round(event.y | 0)
                if (event.type === Gdk.EventType.DOUBLE_BUTTON_PRESS) {
                    this._reason = ActionReason.TILE_DOUBLE_CLICKED
                }
                return false
            }
            return true
        })
        if (this.color) {
            this.gtkAppendDrawOperations([
                new FillImage(0, 0, this._width(this.width), this._height(this.height), this.color)
            ])
        }
        const gtkChild = gtkWidget.getChildren()[0]
        gtkChild.on('draw', (ctx) => {
            this.gtkDraw(gtkChild, ctx)
            return false
        })
        // For mnemonic see accelerator in gtkXml()
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        this._callback && gtkWidget.off('clicked', this._callback)
        this._callback = () => {
            context.setVar('$KEY', new Str(this.key))
            context.setVar('$VALUE', new Str(''))
            context.setVar('$X', new Int(this._x))
            context.setVar('$Y', new Int(this._y))
            context.setVar('$DATA', new Str(this._clientData))
            context.setVar('$REASON', new Int(this._reason))
            VeLispEvaluator.evaluate(unescape(this._action), context)
        }
        gtkWidget.on('clicked', this._callback)
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        const mnemonicChar = this._mnemonicChar(this.mnemonic)
        let accelerator = ''
        if (mnemonicChar) {
            // GDK_MOD1_MASK is Alt
            accelerator =
                `<accelerator key="${mnemonicChar}" signal="clicked" modifiers="GDK_MOD1_MASK"/>`
        }
        return `
<object class="GtkButton" ${id}>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">${this._bool(this.is_tab_stop)}</property>
  <property name="can_default">True</property>
  <property name="has_default">${this._bool(this.is_default)}</property>
  <property name="receives_default">True</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  ${accelerator}
  <child>
    <object class="GtkDrawingArea">
      <property name="width_request">${this._width(this.width)}</property>
      <property name="height_request">${this._height(this.height)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`
    }
}

class PopupList extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        this.alignment = ''
        this.edit_width = 0
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.is_enabled = true
        // TODO: Doesn't work as expected,
        // probably due to internal Gtk.Entry
        //this.is_tab_stop = true
        this.key = null
        this.label = ''
        this.list = ''
        this.mnemonic = ''
        this.tabs = ''
        this.value = ''
        this.width = -1
        // Locals
        this._action = null
        this._callback = null
    }

    gtkInitWidget(gtkWidget) {
        this._action = this.action
        this.gtkSetTile(gtkWidget, this.value)
        const mnemonicLabel = this._mnemonicLabel(this.mnemonic, this.label)
        if (mnemonicLabel) {
            const gtkLabel = gtkWidget.getParent().getChildren()[0]
            gtkLabel.setTextWithMnemonic(mnemonicLabel)
            gtkLabel.setMnemonicWidget(gtkWidget)
        }
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        this._callback && gtkWidget.off('changed', this._callback)
        this._callback = () => {
            context.setVar('$KEY', new Str(this.key))
            // TODO: Should be nil if nothing is selected?
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)))
            context.setVar('$DATA', new Str(this._clientData))
            context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
            VeLispEvaluator.evaluate(unescape(this._action), context)
        }
        gtkWidget.on('changed', this._callback)
    }

    gtkGetTile(gtkWidget) {
        const value = gtkWidget.getActive()
        return value != -1 ? value.toString() : ''
    }

    gtkSetTile(gtkWidget, value) {
        value = Number.parseInt(value)
        if (!Number.isInteger(value)) {
            value = -1
        }
        gtkWidget.setActive(value)
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <!-- Not sure for now how it should align
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  -->
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <child>
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
      <property name="justify">left</property>
      <property name="margin_right">4</property>
    </object>
    <packing>
      <property name="fill">True</property>
      <property name="expand">False</property>
      <property name="pack_type">start</property>
      <property name="position">0</property>
    </packing>
  </child>
  <child>
    <object class="GtkComboBox" ${id}>
      <property name="visible">True</property>
      <property name="sensitive">${this._bool(this.is_enabled)}</property>
      <property name="can_focus">False</property>
      <property name="width_request">${this._width(this.edit_width)}</property>
      <property name="model">liststore-${this.key}</property>
      <child>
        <object class="GtkCellRendererText"/>
        <attributes>
          <attribute name="text">0</attribute>
        </attributes>
      </child>
    </object>
    <packing>
      <property name="fill">True</property>
      <property name="expand">${this._bool(!this.edit_width > 0)}</property>
      <property name="pack_type">end</property>
      <property name="position">1</property>
    </packing>
  </child>
</object>
`
    }
}

class ListBox extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        this.alignment = ''
        //this.allow_accept = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.is_enabled = true
        this.is_tab_stop = true
        this.key = null
        this.label = ''
        this.list = ''
        this.mnemonic = ''
        this.multiple_select = false
        this.tabs = ''
        this.value = ''
        this.width = -1
        // Locals
        this._action = null
        this._callback = null
    }

    gtkInitWidget(gtkWidget) {
        this._action = this.action
        this.gtkSetTile(gtkWidget, this.value)
        const mnemonicLabel = this._mnemonicLabel(this.mnemonic, this.label)
        if (mnemonicLabel) {
            const gtkLabel = gtkWidget.getParent().getParent().getChildren()[0]
            gtkLabel.setTextWithMnemonic(mnemonicLabel)
            gtkLabel.setMnemonicWidget(gtkWidget)
        }
    }

    gtkActionTile(gtkWidget, action, context) {
        const selection = gtkWidget.getSelection()
        this._action = action
        this._callback && selection.off('changed', this._callback)
        this._callback = () => {
            context.setVar('$KEY', new Str(this.key))
            // TODO: Should be nil if nothing is selected?
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)))
            context.setVar('$DATA', new Str(this._clientData))
            // TODO: Implement reason 4 - List Box double-clicked?
            context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
            VeLispEvaluator.evaluate(unescape(this._action), context)
        }
        selection.on('changed', this._callback)
    }

    gtkGetTile(gtkWidget) {
        const selection = gtkWidget.getSelection()
        const [rows, ] = selection.getSelectedRows()
        return rows.map(row => row.toString()).join(' ')
    }

    gtkSetTile(gtkWidget, value) {
        const selection = gtkWidget.getSelection()
        // TODO: Unselect doesn't work for single select
        selection.unselectAll()
        value.split(' ')
            .map(v => Number.parseInt(v))
            .filter(Number.isInteger)
            .forEach(index => {
                const path = new Gtk.TreePath.new()
                path.appendIndex(index)
                selection.selectPath(path)
            })
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        const mode = this.multiple_select ? 'multiple' : 'single'
        const label = `
<child>
  <object class="GtkLabel">
    <property name="visible">True</property>
    <property name="can_focus">False</property>
    <property name="label">${this._escape(this.label)}</property>
    <property name="justify">center</property>
    <property name="margin_bottom">4</property>
  </object>
</child>
`
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <!-- Not sure for now how it should align
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  -->
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>

  <!-- Optional Label -->
  ${this.label ? label : ''}

  <child>
    <object class="GtkScrolledWindow">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="shadow_type">in</property>
      <property name="max_content_width">${this._width(this.width)}</property>
      <property name="max_content_height">${this._height(this.height)}</property>
      <property name="propagate_natural_width">True</property>
      <property name="propagate_natural_height">True</property>
      <child>
        <object class="GtkTreeView" ${id}>
          <property name="visible">True</property>
          <property name="sensitive">${this._bool(this.is_enabled)}</property>
          <property name="can_focus">${this._bool(this.is_tab_stop)}</property>
          <property name="model">liststore-${this.key}</property>
          <property name="headers_visible">False</property>
          <property name="headers_clickable">False</property>
          <property name="enable_search">False</property>
          <property name="show_expanders">False</property>
          <property name="hscroll_policy">natural</property>
          <property name="vscroll_policy">natural</property>
          <child internal-child="selection">
             <object class="GtkTreeSelection">
               <property name="mode">${mode}</property>
             </object>
          </child>
          <child>
            <object class="GtkTreeViewColumn">
              <property name="title">One</property>
              <child>
                <object class="GtkCellRendererText"/>
                  <attributes>
                    <attribute name="text">0</attribute>
                  </attributes>
              </child>
            </object>
          </child>
        </object>
      </child>
    </object>
    <packing>
      <property name="fill">True</property>
      <property name="expand">True</property>
    </packing>
  </child>
</object>
`
    }
}

class RadioCluster extends Cluster {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        // Locals
        this._action = null
        this._callback = null
    }

    gtkInitWidget(_gtkWidget) {
        this._action = this.action
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        for (let i = 0; i < gtkWidget.getChildren().length; i++) {
            const child = gtkWidget.getChildren()[i]
            const tile = this._tiles[i]
            // Ensure it's radio button and it has not action
            if (child instanceof Gtk.RadioButton && !tile.action) {
                tile._callback && child.off('clicked', tile._callback)
                tile._callback = () => {
                    if (child.active) {
                        // Radio Cluster Key
                        context.setVar('$KEY', new Str(this.key))
                        // Radio Button Key
                        context.setVar('$VALUE', new Str(this._tiles[i].key))
                        context.setVar('$DATA', new Str(this._clientData))
                        context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
                        VeLispEvaluator.evaluate(unescape(this._action), context)
                    }
                }
                child.on('clicked', tile._callback)
            }
        }
    }

    gtkGetTile(gtkWidget) {
        for (let i = 0; i < gtkWidget.getChildren().length; i++) {
            const child = gtkWidget.getChildren()[i]
            if (child instanceof Gtk.RadioButton && child.active) {
                return this._tiles[i].key
            }
        }
        return ''
    }

    gtkSetTile(gtkWidget, value) {
        for (let i = 0; i < gtkWidget.getChildren().length; i++) {
            const child = gtkWidget.getChildren()[i]
            if (child instanceof Gtk.RadioButton) {
                this._tiles[i].gtkSetTile(child, value)
            }
        }
    }
}

class RadioRow extends RadioCluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.FILLED
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.key = null
        //this.label = ''
        this.width = -1
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        let group = ''
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key
                break
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW, group}))
        ).join('\n')
        return `
<object class="GtkBox" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">horizontal</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  ${tiles}
</object>
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
    }
}

class RadioColumn extends RadioCluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = ''
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.key = null
        //this.label = ''
        this.width = -1
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        let group = ''
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key
                break
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN, group}))
        ).join('\n')
        return `
<object class="GtkBox" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">vertical</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  ${tiles}
</object>
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
    }
}

class BoxedRadioRow extends RadioCluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.FILLED
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.key = null
        this.label = ''
        this.width = -1
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        let group = ''
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key
                break
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW, group}))
        ).join('\n')
        return `
<object class="GtkFrame">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="label_xalign">0.029999999329447746</property>
  <child>
    <!-- Since it's a radio group, id must be here -->
    <object class="GtkBox" ${id}>
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="orientation">horizontal</property>
      <property name="halign">${this._halign(this.alignment, layout)}</property>
      <property name="valign">${this._valign(this.alignment, layout)}</property>
      <property name="width_request">${this._width(this.width)}</property>
      <property name="height_request">${this._height(this.height)}</property>
      ${tiles}
    </object>
  </child>
  <child type="label">
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
    }
}

class BoxedRadioColumn extends RadioCluster {
    constructor(id) {
        super(id)
        // Attributes
        this.alignment = Alignment.FILLED
        //this.children_alignment = ''
        //this.children_fixed_height = false
        //this.children_fixed_width = false
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.key = null
        this.label = ''
        this.width = -1
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        let group = ''
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key
                break
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN, group}))
        ).join('\n')
        return `
<object class="GtkFrame">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="label_xalign">0.03</property>
  <child>
    <!-- Since it's a radio group, id must be here -->
    <object class="GtkBox" ${id}>
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="orientation">vertical</property>
      <property name="halign">${this._halign(this.alignment, layout)}</property>
      <property name="valign">${this._valign(this.alignment, layout)}</property>
      <property name="width_request">${this._width(this.width)}</property>
      <property name="height_request">${this._height(this.height)}</property>
      ${tiles}
    </object>
  </child>
  <child type="label">
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">True</property>
  <property name="expand">True</property>
</packing>
`
    }
}

class RadioButton extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        this.alignment = ''
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.is_enabled = true
        this.is_tab_stop = true
        this.key = null
        this.label = ''
        this.mnemonic = ''
        this.value = '0'
        this.width = -1
        // Locals
        this._action = null
        this._callback = null
    }

    gtkInitWidget(gtkWidget) {
        this._action = this.action
        const mnemonicLabel = this._mnemonicLabel(this.mnemonic, this.label)
        if (mnemonicLabel) {
            const gtkLabel = gtkWidget.getChildren()[0]
            gtkLabel.setTextWithMnemonic(mnemonicLabel)
            gtkLabel.setMnemonicWidget(gtkWidget)
        }
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        this._callback && gtkWidget.off('clicked', this._callback)
        this._callback = () => {
            if (gtkWidget.active) {
                context.setVar('$KEY', new Str(this.key))
                context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)))
                context.setVar('$DATA', new Str(this._clientData))
                context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
                VeLispEvaluator.evaluate(unescape(this._action), context)
            }
        }
        gtkWidget.on('clicked', this._callback)
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.active ? '1' : '0'
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.active = (value === '1')
    }

    gtkXml({layout, group}) {
        const id = this.key ? `id="${this.key}"` : ''
        return `
<object class="GtkRadioButton" ${id}>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">${this._bool(this.is_tab_stop)}</property>
  <property name="receives_default">False</property>
  <property name="draw_indicator">True</property>
  <property name="group">${group}</property>
  <property name="active">${this._bool(this.value === '1')}</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <child>
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`
    }
}

class Slider extends Tile {
    constructor(id) {
        super(id)
        // Attrubutes
        this.action = ''
        this.alignment = Alignment.FILLED
        //this.big_increment = integer // one-tenth of the total range
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.key = ''
        this.label = ''
        this.layout = 'horizontal' // 'vertical'
        this.max_value = 10000  // signed 16-bit integer no greater than 32767
        this.min_value = 0      // signed 16-bit integer no less than -32768
        this.mnemonic = ''
        //this.small_increment = integer // one one-hundredth the total range
        this.value = 0
        this.width = -1
        // Locals
        this._action = null
        this._callback = null
        this._index = Slider.index++
    }

    gtkInitWidget(_gtkWidget) {
        this._action = this.action
        // For mnemonic see accelerator in gtkXml()
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        this._callback && gtkWidget.off('value-changed', this._callback)
        this._callback = () => {
            context.setVar('$KEY', new Str(this.key))
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)))
            context.setVar('$DATA', new Str(this._clientData))
            // TODO: Implement reason 3 - Slider dragging?
            // Should take into account small big and increments
            // https://help.autodesk.com/view/OARX/2019/ENU/?guid=GUID-F208561B-5C2E-47FE-BD24-520DF75DE92A
            context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
            VeLispEvaluator.evaluate(unescape(this._action), context)
        }
        gtkWidget.on('value-changed', this._callback)
    }

    gtkGetTile(gtkWidget) {
        const value = gtkWidget.getValue()
        return Math.round(value).toString()
    }

    gtkSetTile(gtkWidget, value) {
        value = Number.parseInt(value)
        if (!Number.isInteger(value)) {
            value = 0
        }
        gtkWidget.setValue(value)
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        const mnemonicChar = this._mnemonicChar(this.mnemonic, this.label)
        let accelerator = ''
        if (mnemonicChar) {
            // GDK_MOD1_MASK is Alt
            accelerator =
                `<accelerator key="${mnemonicChar}" signal="grab-focus" modifiers="GDK_MOD1_MASK"/>`
        }
        return `
<object class="GtkScale" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">True</property>
  <property name="orientation">${this.layout}</property>
  <property name="adjustment">scale-adj-${this._index}</property> -->
  <property name="round_digits">0</property>
  <property name="digits">0</property>
  <property name="draw_value">False</property>
  <property name="value_pos">top</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  ${accelerator}
</object>
<packing>
   <property name="expand">True</property>
   <property name="fill">True</property>
</packing>
<object class="GtkAdjustment" id="scale-adj-${this._index}">
  <property name="lower">${this.min_value}</property>
  <property name="upper">${this.max_value}</property>
  <property name="value">${this.value}</property>
  <!-- this.big_increment? -->
  <property name="step_increment">1</property>
</object>
`
    }
}
Slider.index = 0

class Toggle extends Tile {
    constructor(id) {
        super(id)
        // Attributes
        this.action = ''
        this.alignment = ''
        //this.fixed_height = false
        //this.fixed_width = false
        this.height = -1
        this.is_enabled = true
        this.is_tab_stop = true
        this.key = null
        this.label = ''
        this.mnemonic = ''
        this.value = '0'
        this.width = -1
        // Locals
        this._action = null
        this._callback = null
    }

    gtkInitWidget(gtkWidget) {
        this._action = this.action
        const mnemonicLabel = this._mnemonicLabel(this.mnemonic, this.label)
        if (mnemonicLabel) {
            const gtkLabel = gtkWidget.getChildren()[0]
            gtkLabel.setTextWithMnemonic(mnemonicLabel)
            gtkLabel.setMnemonicWidget(gtkWidget)
        }
    }

    gtkActionTile(gtkWidget, action, context) {
        this._action = action
        this._callback && gtkWidget.off('clicked', this._callback)
        this._callback = () => {
            context.setVar('$KEY', new Str(this.key))
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)))
            context.setVar('$DATA', new Str(this._clientData))
            context.setVar('$REASON', new Int(ActionReason.TILE_SELECTED))
            VeLispEvaluator.evaluate(unescape(this._action), context)
        }
        gtkWidget.on('clicked', this._callback)
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.active ? '1' : '0'
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.active = (value === '1')
        this.value = value
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : ''
        return `
<object class="GtkCheckButton" ${id}>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">${this._bool(this.is_tab_stop)}</property>
  <property name="receives_default">False</property>
  <property name="draw_indicator">True</property>
  <property name="active">${this._bool(this.value === '1')}</property>
  <property name="margin_left">4</property>
  <property name="margin_right">4</property>
  <property name="margin_top">4</property>
  <property name="margin_bottom">4</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <child>
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this._escape(this.label)}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`
    }
}

const tileCtors = {
    // Clusters
    'dialog'            : (id) => new Dialog(id),
    'row'               : (id) => new Row(id),
    'column'            : (id) => new Column(id),
    'boxed_row'         : (id) => new BoxedRow(id),
    'boxed_column'      : (id) => new BoxedColumn(id),
    'concatenation'     : (id) => new Concatenation(id),
    'paragraph'         : (id) => new Paragraph(id),
    'radio_row'         : (id) => new RadioRow(id),
    'radio_column'      : (id) => new RadioColumn(id),
    'boxed_radio_row'   : (id) => new BoxedRadioRow(id),
    'boxed_radio_column': (id) => new BoxedRadioColumn(id),
    // Tiles
    'button'            : (id) => new Button(id),
    'edit_box'          : (id) => new EditBox(id),
    'image'             : (id) => new Image(id),
    'image_button'      : (id) => new ImageButton(id),
    'list_box'          : (id) => new ListBox(id),
    'popup_list'        : (id) => new PopupList(id),
    'radio_button'      : (id) => new RadioButton(id),
    'slider'            : (id) => new Slider(id),
    'spacer'            : (id) => new Spacer(id),
    'text'              : (id) => new Text(id),
    'text_part'         : (id) => new TextPart(id),
    'toggle'            : (id) => new Toggle(id),
}

const buildTile = (tileName, tileId) => {
    const tileCtor = tileCtors[tileName]
    if (!tileCtor) {
        throw new Error(`Unknown tile constructor: ${tileName}`)
    }
    return tileCtor(tileId)
}

export {
    Dialog,
    ListOperation,
    buildTile,
}

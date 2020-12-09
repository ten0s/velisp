const {Str} = require('./VeLispTypes.js');
const Evaluator = require('./VeLispEvaluator.js');

const gi = require('node-gtk');
const Gtk = gi.require('Gtk', '3.0');
const GObject = gi.require('GObject');

gi.startLoop();
Gtk.init();

const TileMode = {
    ENABLE_TILE: 0,
    DISABLE_TILE: 1,
    FOCUS_TILE: 2,
    SELECT_EDITBOX: 3,
    FLIP_IMAGE: 4,
};

const Layout = {
    COLUMN: 'column',
    ROW: 'row',
};

const Alignment = {
    LEFT: 'left',
    RIGHT: 'right',
    TOP: 'top',
    BOTTOM: 'bottom',
    CENTERED: 'centered',
    FILLED: 'filled', // GTK specific
}

const ListOperation = {
    CHANGE: 1,
    APPEND: 2,
    CLEAR: 3,
}

class Tile {
    constructor(id) {
        this.id = id;
    }

    addAttribute(name, value) {
        if (this.hasOwnProperty(name)) {
            this[name] = value;
            return true;
        }
        return false;
    }

    clone() {
        return Object.assign(Object.create(Object.getPrototypeOf(this)), this);
    }

    _child(xml) {
        return `<child>${xml}</child>`;
    }

    _bool(value) {
        return value ? 'True' : 'False';
    }

    _bold(value) {
        return value ? 'bold' : 'normal';
    }

    _dcl_to_gtk_align(alignment) {
        switch (alignment) {
        case Alignment.LEFT:
            return 'start';
        case Alignment.RIGHT:
            return 'end';
        case Alignment.TOP:
            return 'start';
        case Alignment.BOTTOM:
            return 'end';
        case Alignment.CENTERED:
            return 'center';
        case Alignment.FILLED:
            return 'fill';
        default:
            throw new Error(`Invalid alignment: ${alignment}`);
        }
    }

    _halign(alignment, layout) {
        switch (layout) {
        case Layout.COLUMN:
            return this._dcl_to_gtk_align(alignment ? alignment : Alignment.LEFT);
        case Layout.ROW:
            return 'fill';
        default:
            throw new Error(`Invalid layout: ${layout}`);
        }
    }

    _valign(alignment, layout) {
        switch (layout) {
        case Layout.COLUMN:
            return 'fill';
        case Layout.ROW:
            return this._dcl_to_gtk_align(alignment ? alignment : Alignment.CENTERED);
        default:
            throw new Error(`Invalid layout: ${layout}`);
        }
    }

    _width(value) {
        if (value === -1) {
            return value;
        }
        // TODO: Calculate based on font
        // https://help.autodesk.com/view/OARX/2019/ENU/?guid=GUID-F5ACFAA1-4528-4BC5-B45E-0F7C114AFEDE
        return 7.5 * value;
    }

    _height(value) {
        if (value === -1) {
            return value;
        }
        // TODO: Calculate based on font
        // https://help.autodesk.com/view/OARX/2019/ENU/?guid=GUID-DD0E03A8-D2EB-4D79-9BF7-D49EFD3820D4
        return 15 * value;
    }

    // Optional
    gtkInitWidget(gtkWidget) {
    }

    gtkActionTile(gtkWidget, handler, context) {
        throw new Error(
            `Not implemented gtkActionTile for ${this.constructor.name}`
        );
    }

    gtkGetTile(gtkWidget) {
        throw new Error(
            `Not implemented gtkGetTile for ${this.constructor.name}`
        );
    }

    gtkSetTile(gtkWidget, value) {
        throw new Error(
            `Not implemented gtkSetTile for ${this.constructor.name}`
        );
    }

    gtkDimX(gtkWidget) {
        throw new Error(
            `Not implemented gtkDimX for ${this.constructor.name}`
        );
    }

    gtkDimY(gtkWidget) {
        throw new Error(
            `Not implemented gtkDimY for ${this.constructor.name}`
        );
    }
    gtkXml() {
        throw new Error(
            'Not implemented gtkXml for ${this.constructor.name}'
        );
    }
}

class Cluster extends Tile {
    constructor(id) {
        super(id);
        this._tiles = [];
    }

    addTile(tile) {
        this._tiles.push(tile);
    }

    getKeys() {
        let list = [];
        for (let tile of this._tiles) {
            if (tile instanceof Cluster) {
                list = list.concat(tile.getKeys());
            } else {
                if (tile.key) {
                    list.push(tile.key);
                }
            }
        }
        return list;
    }

    getActions() {
        let list = [];
        for (let tile of this._tiles) {
            if (tile instanceof Cluster) {
                list = list.concat(tile.getActions());
            } else {
                if (tile.action && tile.key) {
                    list.push([tile.key, tile.action]);
                }
            }
        }
        return list;
    }

    getListStores() {
        let list = [];
        for (let tile of this._tiles) {
            if (tile instanceof Cluster) {
                list = list.concat(tile.getListStores());
            } else {
                if (tile.hasOwnProperty('list') && tile.key) {
                    list.push([tile.key, tile.list, tile.tabs]);
                }
            }
        }
        return list;
    }

    findTile(key) {
        if (this.key === key) {
            return this;
        }
        for (let tile of this._tiles) {
            if (tile.key === key) {
                return tile;
            }
            if (tile instanceof Cluster) {
                const found = tile.findTile(key);
                if (found) {
                    return found;
                }
            }
        }
        return null;
    }
}

class Dialog extends Cluster {
    constructor(id) {
        super(id);
        //this.initial_focus = null;
        this.key = null;
        this.label = '';
        this.value = '';
        delete this._tiles;
        this._column = new Column();
        this._gtkBuilder = null;
        this._gtkWindow = null;
        this._listStores = null; // [ [key, list, tabs] ]
    }

    // Cluster
    addTile(tile) {
        this._column.addTile(tile);
    }

    // Cluster
    getKeys() {
        return this._column.getKeys();
    }

    // Cluster
    getActions() {
        return this._column.getActions();
    }

    // Cluster
    getListStores() {
        return this._column.getListStores();
    }

    // Cluster
    findTile(key) {
        const tile = this._column.findTile(key);
        if (tile) {
            return tile;
        }
        throw new Error(`Tile not found: ${key}`);
    }

    // DCL
    actionTile(key, handler, context) {
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        tile.gtkActionTile(gtkWidget, handler, context);
    }

    // DCL
    getTile(key) {
        if (this.key === key) {
            return this.gtkGetTile(this._gtkWindow);
        }
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        return tile.gtkGetTile(gtkWidget);
    }

    // DCL
    setTile(key, value) {
        if (this.key === key) {
            return this.gtkSetTile(this._gtkWindow, value);
        }
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        return tile.gtkSetTile(gtkWidget, value);
    }

    // DCL
    modeTile(key, mode) {
        const gtkWidget = this.gtkFindWidget(key);
        switch (mode) {
        case TileMode.ENABLE_TILE:
            gtkWidget.setSensitive(true);
            break;
        case TileMode.DISABLE_TILE:
            gtkWidget.setSensitive(false);
            break;
        case TileMode.FOCUS_TILE:
            this._gtkWindow.setFocus(gtkWidget);
            break;
        case TileMode.FLIP_IMAGE:
            console.error(`Error: not implemented tile mode '${mode}'`);
            break;
        default:
            console.error(`Error: unknown tile mode '${mode}'`);
        }
    }

    // DCL
    startDialog() {
        this._gtkWindow.setModal(true);
        this._gtkWindow.setResizable(false);
        // TODO: calculate using both length and font
        console.log(this._gtkWindow.getTitle().length);
        const fixMeWidth = this._gtkWindow.getTitle().length * 8.4 + 170;
        this._gtkWindow.setSizeRequest(fixMeWidth, -1);
        this._gtkWindow.on('show', Gtk.main);
        this._gtkWindow.on('destroy', Gtk.mainQuit);
        this._gtkWindow.showAll();
        // See doneDialog for dialogStatus
        const status = this._status ? this._status : 0;
        return status;
    }

    // DCL
    doneDialog(status) {
        // See startDialog for dialogStatus
        this._status = status;
        Gtk.mainQuit();
        // TODO: what it should return? some (X, Y) point of the dialog
    }

    // DCL
    startList(key, operation, index) {
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        const listStore = gtkWidget.getModel();
        switch (operation) {
        case ListOperation.CHANGE:
            return {
                operation,
                index,
                listStore,
            };
        case ListOperation.APPEND:
            return {
                operation,
                listStore,
            };
        case ListOperation.CLEAR:
            this.gtkListClear(listStore);
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
            this.gtkListChange(listStore, index, str);
            break;
        case ListOperation.APPEND:
            this.gtkListAppend(listStore, str);
            break;
        default:
            throw new Error(`Unknown add_list operation: ${operation}`)
        }
    }

    gtkListClear(listStore) {
        listStore.clear();
    }

    gtkListChange(listStore, index, str) {
        debugger;
        const path = new Gtk.TreePath.newFromIndices([index]);
        const [valid, iter] = listStore.getIter(path);
        if (!valid) {
            throw new Error(`Invalid add_list index: ${index}`);
        }
        this.gtkListSet(listStore, iter, str);
    }

    gtkListAppend(listStore, str) {
        const iter = listStore.append();
        this.gtkListSet(listStore, iter, str);
    }

    gtkListSet(listStore, iter, str) {
        const value = new GObject.Value();
        value.init(GObject.TYPE_STRING);
        value.setString(str);
        listStore.setValue(iter, 0, value);
    }

    // DCL
    endList({}) {
    }

    // DCL
    dimX(key) {
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        return tile.gtkDimX(gtkWidget);
    }

    // DCL
    dimY(key) {
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        return tile.gtkDimY(gtkWidget);
    }

    // DCL
    startImage(key) {
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        return {
            tile,
            operations: [],
        };
    }

    // DCL
    fillImage(handle, x, y, w, h, color) {
        handle.operations.push(["fill_image", x, y, w, h, color]);
    }

    // DCL
    vectorImage(handle, x1, y1, x2, y2, color) {
        handle.operations.push(["vector_image", x1, y1, x2, y2, color]);
    }

    // DCL
    endImage({tile, operations}) {
        tile.setOperations(operations);
    }

    // GTK
    gtkInitWidget(context) {
        // Pre init
        this._listStores = this.getListStores();
        // Init
        const gtkXml = this.gtkXml();
        console.log(gtkXml);
        this._gtkBuilder = new Gtk.Builder();
        this._gtkBuilder.addFromString(gtkXml, gtkXml.length);
        this._gtkWindow = this.gtkFindWidget(this.id);
        // Post init:
        // 1. Init widgets
        for (let key of this.getKeys()) {
            const tile = this.findTile(key);
            const gtkWidget = this.gtkFindWidget(key);
            tile.gtkInitWidget(gtkWidget);
        }
        // 2. Init actions
        for (let [key, handler] of this.getActions()) {
            this.actionTile(key, handler, context);
        }
    }

    gtkFindWidget(key) {
        if (this.key === key) {
            return this._gtkWindow;
        }
        const gtkWidget = this._gtkBuilder.getObject(key);
        if (gtkWidget) {
            return gtkWidget;
        }
        throw new Error(`Widget not found: ${key}`);
    }

    gtkGetTile(gtkWidget) {
        // MUST BE gtkWidget === this._gtkWindow
        return gtkWidget.getTitle();
    }

    gtkSetTile(gtkWidget, value) {
        // MUST BE gtkWidget === this._gtkWindow
        gtkWidget.setTitle(value);
    }

    gtkXml() {
        const id = this.id ? `id="${this.id}"` : '';
        const title = this.label ? this.label : this.value;
        const child = this._child(this._column.gtkXml({layout: Layout.COLUMN}));
        const listStores = this._listStores.map(([key, list, tabs]) => {
            return (new  ListStore(key, list, tabs)).gtkXml();
        }).join('\n');
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
`;
    }
}

class ListStore {
    constructor(key, list, tabs) {
        this.key = key;
        this.list = list;
        this.tabs = tabs;
    }

    _column() {
        return `<column type="gchararray"/>`;
    }

    _row(value) {
        return `<row><col id="0">${value}</col></row>`;
    }

    gtkXml() {
        const rows = this.list.split('\\n').map(this._row).join('\n');
        return `
<object class="GtkListStore" id="liststore-${this.key}">
  <columns>
    ${this._column()}
  </columns>
  <data>
    ${rows}
  </data>
</object>
`;
    }
}

class Row extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = Alignment.FILLED;
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        //this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW}))
        ).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
  <!-- Without a border around it -->
  <property name="margin_left">0</property>
  <property name="margin_right">0</property>
  <property name="margin_top">0</property>
  <property name="margin_bottom">0</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <!-- Whether the children should all be the same size??? -->
  <property name="homogeneous">True</property>
  ${tiles}
</object>
`;
    }
}

class Column extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = Alignment.FILLED;
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        //this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN}))
        ).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
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
`;
    }
}

class BoxedRow extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = Alignment.FILLED;
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW}))
        ).join('\n');
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
      <property name="spacing">0</property>
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
      <property name="label">${this.label}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class BoxedColumn extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = Alignment.FILLED;
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN}))
        ).join('\n');
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
      <property name="spacing">0</property>
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
      <property name="label">${this.label}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class Concatenation extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = Alignment.CENTERED;
        this.height = -1;
        this.width = -1;
    }

    gtkXml({layout}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW}))
        ).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
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
`;
    }
}

class Paragraph extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = Alignment.CENTERED;
        this.height = -1;
        this.width = -1;
    }

    gtkXml({layout}) {
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN}))
        ).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
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
`;
    }
}

class Spacer extends Tile {
    constructor(id) {
        super(id);
        this.alignment = '';
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.width = -1;
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
`;
    }
}

class Text extends Tile {
    constructor(id) {
        super(id);
        this.alignment = '';
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.is_bold = false;
        this.key = null;
        this.label = '';
        this.value = '';
        this.width = -1;
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getText();
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value);
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        const label = this.label ? this.label : this.value;
        return `
<object class="GtkLabel" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label">${label}</property>
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
`;
    }
}

class TextPart extends Tile {
    constructor(id) {
        super(id);
        this.alignment = Alignment.CENTERED;
        this.height = -1;
        this.is_bold = false;
        this.key = null;
        this.label = '';
        this.value = '';
        this.width = -1;
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getText();
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value);
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        const label = this.label ? this.label : this.value;
        return `
<object class="GtkLabel" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label">${label}</property>
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
`;
    }
}

class Button extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        //this.is_cancel = false;
        this.is_default = false;
        this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        //this.mnemonic = '';
        this.width = -1;
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        gtkWidget.on('clicked', () => {
            context.setVar('$KEY', new Str(this.key));
            context.setVar('$VALUE', new Str(''));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getLabel();
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setLabel(value);
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">True</property>
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
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class EditBox extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.allow_accept = false;
        this.edit_limit = 132;
        this.edit_width = 0;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        //this.mnemonic = '';
        this.value = '';
        this.width = -1;
        //this.password_char = '*';
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        gtkWidget.on('changed', () => {
            context.setVar('$KEY', new Str(this.key));
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getText();
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value);
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
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
      <property name="label">${this.label}</property>
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
    <object class="GtkEntry" ${id}>
      <property name="visible">True</property>
      <property name="sensitive">${this._bool(this.is_enabled)}</property>
      <property name="can_focus">True</property>
      <property name="max_length">${this.edit_limit}</property>
      <property name="width_chars">${this.edit_width}</property>
      <property name="text">${this.value}</property>
    </object>
    <packing>
      <property name="fill">True</property>
      <property name="expand">${this._bool(!this.edit_width > 0)}</property>
      <property name="pack_type">end</property>
      <property name="position">1</property>
    </packing>
  </child>
</object>
`;
    }
}

class Image extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.aspect_radio = ;
        //this.color;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = 10;
        this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        //this.mnemonic = '';
        this.value = '';
        this.width = 10;
        this._operations = [];
    }

    setOperations(operations) {
        this._operations = operations;
    }

    gtkInitWidget(gtkWidget) {
        gtkWidget.on('draw', (ctx) => this.gtkDraw(gtkWidget, ctx));
    }

    gtkDraw(gtkWidget, gtkCtx) {
        debugger;
        this._operations.forEach(op => this.gtkOperation(gtkWidget, gtkCtx, op));
    }

    gtkOperation(gtkWidget, gtkCtx, op) {
        switch (op[0]) {
        case "fill_image":
            this.gtkFillImage(gtkWidget, gtkCtx, op[1], op[2], op[3], op[4], op[5]);
            break;
        case "vector_image":
            this.gtkVectorImage(gtkWidget, gtkCtx, op[1], op[2], op[3], op[4], op[5]);
            break;
        default:
            throw new Error(`Unknown image operation: ${op[0]}`);
        }
    }

    gtkFillImage(gtkWidget, gtkCtx, x, y, w, h, color) {
        console.log(x, y, w, h, color);
        // TODO: set color
        gtkCtx.setSourceRgba(0, 0, 0, 1);
        gtkCtx.rectangle(this._width(x), this._height(y), this._width(w), this._height(h));
        gtkCtx.fill();
    }

    gtkVectorImage(gtkWidget, gtkCtx, x1, y1, x2, y2, color) {
        console.log(x1, y1, x2, y2, color);
        // TODO: set color
        gtkCtx.setSourceRgb(0, 0, 0);
        gtkCtx.setLineWidth(1);
        gtkCtx.moveTo(this._width(x1), this._height(y1));
        gtkCtx.lineTo(this._width(x2), this._height(y2));
        gtkCtx.stroke();
    }

    /*
    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        gtkWidget.on('changed', () => {
            context.setVar('$KEY', new Str(this.key));
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }
    */

    /*
    gtkGetTile(gtkWidget) {
        return gtkWidget.getText();
    }
    */

    /*
    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value);
    }
    */

    gtkDimX(gtkWidget) {
        //return gtkWidget.getAllocatedWidth();
        return this.width;
    }

    gtkDimY(gtkWidget) {
        //return gtkWidget.getAllocatedHeight();
        return this.height;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
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
      <property name="can_focus">False</property>
      <property name="sensitive">${this._bool(this.is_enabled)}</property>
    </object>
    <packing>
       <property name="expand">True</property>
       <property name="fill">True</property>
    </packing>
  </child>
</object>
`;
    }
}

class PopupList extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        this.edit_width = 0;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        this.list = '';
        //this.mnemonic = '';
        this.tabs = '';
        this.value = '';
        this.width = -1;
    }

    gtkInitWidget(gtkWidget) {
        this.gtkSetTile(gtkWidget, this.value);
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        gtkWidget.on('changed', () => {
            context.setVar('$KEY', new Str(this.key));
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }

    gtkGetTile(gtkWidget) {
        const value = gtkWidget.getActive();
        return value != -1 ? value.toString() : '';
    }

    gtkSetTile(gtkWidget, value) {
        value = Number.parseInt(value);
        if (!Number.isInteger(value)) {
            value = -1;
        }
        gtkWidget.setActive(value);
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
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
      <property name="label">${this.label}</property>
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
      <property name="can_focus">True</property>
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
`;
    }
}

class ListBox extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.allow_accept = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        this.list = '';
        //this.mnemonic = '';
        this.multiple_select = false;
        this.tabs = '';
        this.value = '';
        this.width = -1;
    }

    gtkInitWidget(gtkWidget) {
        this.gtkSetTile(gtkWidget, this.value);
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        const selection = gtkWidget.getSelection();
        selection.on('changed', () => {
            context.setVar('$KEY', new Str(this.key));
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }

    gtkGetTile(gtkWidget) {
        const selection = gtkWidget.getSelection();
        const [rows, ] = selection.getSelectedRows();
        return rows.map(row => row.toString()).join(' ');
    }
     
    gtkSetTile(gtkWidget, value) {
        const selection = gtkWidget.getSelection();
        // TODO: support unselect for single
        selection.unselectAll();
        value.split(' ')
             .map(v => Number.parseInt(v))
             .filter(Number.isInteger)
             .forEach(index => {
                 const path = new Gtk.TreePath.newFromIndices([index]);
                 selection.selectPath(path);
             });
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        const mode = this.multiple_select ? 'multiple' : 'single';
        const label = `
<child>
  <object class="GtkLabel">
    <property name="visible">True</property>
    <property name="can_focus">False</property>
    <property name="label">${this.label}</property>
    <property name="justify">center</property>
    <property name="margin_bottom">4</property>
  </object>
</child>
`;
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
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
    <object class="GtkTreeView" ${id}>
      <property name="visible">True</property>
      <property name="sensitive">${this._bool(this.is_enabled)}</property>
      <property name="can_focus">True</property>
      <property name="model">liststore-${this.key}</property>
      <property name="headers_visible">False</property>
      <property name="headers_clickable">False</property>
      <property name="enable_search">False</property>
      <property name="show_expanders">False</property>
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
`;
    }
}

class RadioCluster extends Cluster {
    constructor(id) {
        super(id);
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        for (let i = 0; i < gtkWidget.getChildren().length; i++) {
            const child = gtkWidget.getChildren()[i];
            // Ensure it's radio button and it has not action
            if (child instanceof Gtk.RadioButton && this._tiles[i].action === '') {
                child.on('clicked', () => {
                    if (child.active) {
                        // Radio Cluster Key
                        context.setVar('$KEY', new Str(this.key));
                        // Radio Button Key
                        context.setVar('$VALUE', new Str(this._tiles[i].key));
                        Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
                    }
                });
            }
        }
    }

    gtkGetTile(gtkWidget) {
        for (let i = 0; i < gtkWidget.getChildren().length; i++) {
            const child = gtkWidget.getChildren()[i];
            if (child instanceof Gtk.RadioButton && child.active) {
                return this._tiles[i].key;
            }
        }
        return '';
    }

    gtkSetTile(gtkWidget, value) {
        for (let i = 0; i < gtkWidget.getChildren().length; i++) {
            const child = gtkWidget.getChildren()[i];
            if (child instanceof Gtk.RadioButton) {
                this._tiles[i].gtkSetTile(child, value);
            }
        }
    }
}

class RadioRow extends RadioCluster {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = Alignment.FILLED;
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        //this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW, group}))
        ).join('\n');
        return `
<object class="GtkBox" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">horizontal</property>
  <property name="spacing">0</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  ${tiles}
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class RadioColumn extends RadioCluster {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        //this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN, group}))
        ).join('\n');
        return `
<object class="GtkBox" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">vertical</property>
  <property name="spacing">0</property>
  <property name="halign">${this._halign(this.alignment, layout)}</property>
  <property name="valign">${this._valign(this.alignment, layout)}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  ${tiles}
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class BoxedRadioRow extends RadioCluster {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = Alignment.FILLED;
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.ROW, group}))
        ).join('\n');
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
      <property name="spacing">0</property>
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
      <property name="label">${this.label}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class BoxedRadioColumn extends RadioCluster {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = Alignment.FILLED;
        //this.children_alignment = '';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        this.label = '';
        this.width = -1;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(
            tile => this._child(tile.gtkXml({layout: Layout.COLUMN, group}))
        ).join('\n');
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
      <property name="spacing">0</property>
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
      <property name="label">${this.label}</property>
    </object>
  </child>
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class RadioButton extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        //this.mnemonic = '';
        this.value = '0';
        this.width = -1;
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        gtkWidget.on('clicked', () => {
            if (gtkWidget.active) {
                context.setVar('$KEY', new Str(this.key));
                context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)));
                Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
            }
        });
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.active ? '1' : '0';
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.active = (value === '1');
    }

    gtkXml({layout, group}) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkRadioButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">True</property>
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
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class Slider extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.big_increment = integer; // one-tenth of the total range
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = '';
        //this.label = '';
        this.layout = 'horizontal'; // 'vertical'
        this.max_value = 10000;  // signed 16-bit integer no greater than 32767
        this.min_value = 0;      // signed 16-bit integer no less than -32768
        //this.mnemonic = '';
        //this.small_increment = integer; // one one-hundredth the total range
        this.value = 0;
        this.width = -1;
        this._index = Slider.index++;
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        gtkWidget.on('value-changed', () => {
            context.setVar('$KEY', new Str(this.key));
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }

    gtkGetTile(gtkWidget) {
        const value = gtkWidget.getValue();
        return Math.round(value).toString();
    }

    gtkSetTile(gtkWidget, value) {
        value = Number.parseInt(value);
        if (!Number.isInteger(value)) {
            value = 0;
        }
        gtkWidget.setValue(value);
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
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
</object>
<packing>
   <property name="expand">False</property>
   <property name="fill">False</property>
</packing>
<object class="GtkAdjustment" id="scale-adj-${this._index}">
  <property name="lower">${this.min_value}</property>
  <property name="upper">${this.max_value}</property>
  <property name="value">${this.value}</property>
  <!-- this.big_increment? -->
  <property name="step_increment">1</property>
</object>
`;
    }
}
Slider.index = 0;

class Toggle extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = '';
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        //this.mnemonic = '';
        this.value = '0';
        this.width = -1;
    }

    gtkActionTile(gtkWidget, handler, context) {
        this.action = handler;
        gtkWidget.on('clicked', () => {
            context.setVar('$KEY', new Str(this.key));
            context.setVar('$VALUE', new Str(this.gtkGetTile(gtkWidget)));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.active ? '1' : '0';
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.active = (value === '1');
        this.value = value;
    }

    gtkXml({layout}) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkCheckButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">True</property>
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
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

exports.Dialog = Dialog;
exports.ListOperation = ListOperation;

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
    'list_box'          : (id) => new ListBox(id),
    'popup_list'        : (id) => new PopupList(id),
    'radio_button'      : (id) => new RadioButton(id),
    'slider'            : (id) => new Slider(id),
    'spacer'            : (id) => new Spacer(id),
    'text'              : (id) => new Text(id),
    'text_part'         : (id) => new TextPart(id),
    'toggle'            : (id) => new Toggle(id),
};

exports.buildTile = (tileName, tileId) => {
    const tileCtor = tileCtors[tileName];
    if (!tileCtor) {
        throw new Error(`Unknown tile constructor: ${tileName}`);
    }
    return tileCtor(tileId);
};

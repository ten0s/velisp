const {Str} = require('./VeLispTypes.js');
const Evaluator = require('./VeLispEvaluator.js');

const gi = require('node-gtk');
const Gtk = gi.require('Gtk', '3.0');

gi.startLoop();
Gtk.init();

const TileMode = {
    ENABLE_TILE: 0,
    DISABLE_TILE: 1,
    FOCUS_TILE: 2,
    SELECT_EDITBOX: 3,
    FLIP_IMAGE: 4
};

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
        if (value) {
            return 'True';
        }
        return 'False';
    }

    _hor_align(value) {
        switch (value) {
        case 'left':
            return 'start';
        case 'right':
            return 'end';
        case 'centered':
            return 'center';
        default:
            return 'fill';
        }
    }

    _ver_align(value) {
        switch (value) {
        case 'top':
            return 'start';
        case 'bottom':
            return 'end';
        case 'centered':
            return 'center';
        default:
            return 'fill';
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

    gtkSetTile(gtkWidget, _value) {
        throw new Error(
            `Not implemented gtkSetTile for ${this.constructor.name}`
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
        this._column.alignment = 'filled';
        this._gtkBuilder = null;
        this._gtkWindow = null;
    }

    // Cluster
    addTile(tile) {
        this._column.addTile(tile);
    }

    // Cluster
    getActions() {
        return this._column.getActions();
    }

    // Cluster
    findTile(key) {
        return this._column.findTile(key);
    }

    // DCL
    actionTile(key, handler, context) {
        const tile = this.findTile(key);
        const gtkWidget = this.gtkFindWidget(key);
        console.log(gtkWidget);
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

    // GTK
    gtkInitWidget(context) {
        const gtkXml = this.gtkXml();
        console.log(gtkXml);
        this._gtkBuilder = new Gtk.Builder();
        this._gtkBuilder.addFromString(gtkXml, gtkXml.length);
        this._gtkWindow = this.gtkFindWidget(this.id);
        for (let [key, handler] of this.getActions()) {
            this.actionTile(key, handler, context);
        }
    }

    gtkFindWidget(key) {
        if (this.key === key) {
            return this._gtkWindow;
        }
        return this._gtkBuilder.getObject(key);
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
        const child = this._child(this._column.gtkXml());
        return `
<?xml version="1.0" encoding="UTF-8"?>
<interface>
  <requires lib="gtk+" version="3.20"/>
  <object class="GtkWindow" ${id}>
    <property name="can_focus">False</property>
    <property name="title">${title}</property>
    ${child}
  </object>
</interface>
`;
    }
}

class Row extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = 'centered';
        //this.children_alignment = 'centered';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        //this.label = '';
        this.width = -1;
    }

    gtkXml() {
        const tiles = this._tiles.map(tile => this._child(tile.gtkXml())).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>

  <property name="halign">fill</property>
  <property name="valign">${this._ver_align(this.alignment)}</property>

  <!-- Whether the children should all be the same size -->
  <property name="homogeneous">True</property>
  ${tiles}
</object>
`;
    }
}

class Column extends Cluster {
    constructor(id) {
        super(id);
        this.alignment = 'left';
        //this.children_alignment = 'left';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        //this.label = '';
        this.width = -1;
    }

    gtkXml() {
        const tiles = this._tiles.map(tile => this._child(tile.gtkXml())).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>

  <!-- Whether the children should all be the same size -->
  <property name="homogeneous">False</property>
  ${tiles}
</object>
`;
    }
}

class Concatenation extends Column {
}

class Spacer extends Tile {
    constructor(id) {
        super(id);
        this.alignment = 'centered';
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.width = -1;
    }

    gtkXml() {
        return `
<object class="GtkLabel">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label"></property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>
</object>
`;
    }
}

class Text extends Tile {
    constructor(id) {
        super(id);
        this.alignment = 'centered';
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        //this.is_bold = false;
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

    gtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        const label = this.label ? this.label : this.value;
        return `
<object class="GtkLabel" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label">${label}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <property name="margin_left">5</property>
  <property name="margin_right">5</property>
  <property name="margin_top">5</property>
  <property name="margin_bottom">5</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>
</object>
`;
    }
}

class Button extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = 'centered';
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

    // TODO: rename gtkXml
    gtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">True</property>
  <property name="can_default">True</property>
  <property name="has_default">${this._bool(this.is_default)}</property>
  <property name="receives_default">True</property>
  <property name="margin_left">5</property>
  <property name="margin_right">5</property>
  <property name="margin_top">5</property>
  <property name="margin_bottom">5</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>
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
        this.alignment = 'centered';
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
            context.setVar('$VALUE', new Str(gtkWidget.getText ? gtkWidget.getText() : ''));
            Evaluator.evaluate(new Str(this.action).toUnescapedString(), context);
        });
    }

    gtkGetTile(gtkWidget) {
        return gtkWidget.getText();
    }

    gtkSetTile(gtkWidget, value) {
        gtkWidget.setText(value);
    }

    gtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkBox">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <property name="margin_left">5</property>
  <property name="margin_right">5</property>
  <property name="margin_top">5</property>
  <property name="margin_bottom">5</property>
  <child>
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this.label}</property>
      <property name="justify">left</property>
      <property name="margin_right">5</property>
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
        this.alignment = 'centered';
        //this.children_alignment = 'centered';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        //this.label = '';
        this.width = -1;
    }

    gtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(tile => this._child(tile.gtkXml(group))).join('\n');
        return `
<object class="GtkBox" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">horizontal</property>
  <property name="spacing">0</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <property name="halign">${this._hor_align(this.alignment)}</property>
<!--  <property name="valign">${this._ver_align(this.alignment)}</property> -->
  <property name="homogeneous">True</property>
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
        this.alignment = 'left';
        //this.children_alignment = 'left';
        //this.children_fixed_height = false;
        //this.children_fixed_width = false;
        //this.fixed_height = false;
        //this.fixed_width = false;
        this.height = -1;
        this.key = null;
        //this.label = '';
        this.width = -1;
    }

    gtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(tile => this._child(tile.gtkXml(group))).join('\n');
        return `
<object class="GtkBox" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">vertical</property>
  <property name="spacing">0</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>

  ${tiles}
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
        this.alignment = 'centered';
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
        this.value = value;
    }

    gtkXml(radio) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkRadioButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">True</property>
  <property name="receives_default">False</property>
  <property name="draw_indicator">True</property>
  <property name="group">${radio}</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <property name="margin_left">5</property>
  <property name="margin_right">5</property>
  <property name="margin_top">5</property>
  <property name="margin_bottom">5</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>

</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

class Toggle extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.alignment = 'left';
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
        this.value = value;
    }

    gtkXml(radio) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkCheckButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="visible">True</property>
  <property name="sensitive">${this._bool(this.is_enabled)}</property>
  <property name="can_focus">True</property>
  <property name="receives_default">False</property>
  <property name="draw_indicator">True</property>
  <property name="width_request">${this._width(this.width)}</property>
  <property name="height_request">${this._height(this.height)}</property>
  <property name="margin_left">5</property>
  <property name="margin_right">5</property>
  <property name="margin_top">5</property>
  <property name="margin_bottom">5</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>

</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
`;
    }
}

exports.Dialog = Dialog;
exports.Row = Row;
exports.Column = Column;
exports.Concatenation = Concatenation;
exports.Spacer = Spacer;
exports.Text = Text;
exports.Button = Button;
exports.EditBox = EditBox;

exports.RadioRow = RadioRow;
exports.RadioColumn = RadioColumn;
exports.RadioButton = RadioButton;

exports.Toggle = Toggle;

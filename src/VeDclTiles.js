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

    toGtkXml() {
        throw new Error('Not implemented');
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
        this._column.alignment = 'centered';
    }

    addTile(tile) {
        this._column.addTile(tile);
    }

    getActions() {
        return this._column.getActions();
    }

    toGtkXml() {
        const id = this.id ? `id="${this.id}"` : '';
        const title = this.label ? this.label : this.value;
        const child = this._child(this._column.toGtkXml());
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

    toGtkXml() {
        const tiles = this._tiles.map(tile => this._child(tile.toGtkXml())).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">horizontal</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>

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

    toGtkXml() {
        const tiles = this._tiles.map(tile => this._child(tile.toGtkXml())).join('\n');
        return `
<object class="GtkBox">
  <property name="orientation">vertical</property>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>

  <property name="halign">${this._hor_align(this.alignment)}</property>
  <property name="valign">fill</property>

  <!-- Whether the children should all be the same size -->
  <property name="homogeneous">False</property>
  ${tiles}
</object>
<packing>
  <property name="fill">False</property>
  <property name="expand">False</property>
</packing>
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

    toGtkXml() {
        return `
<object class="GtkLabel">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label"></property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>

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

    toGtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        const label = this.label ? this.label : this.value;
        return `
<object class="GtkLabel" ${id}>
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="label">${label}</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>

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
        //this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        //this.mnemonic = '';
        this.width = -1;
    }

    toGtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>
  <property name="visible">True</property>
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
        //this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        //this.mnemonic = '';
        this.value = '';
        this.width = -1;
        //this.password_char = '*';
    }

    toGtkXml() {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkBox">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="spacing">0</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>
  <child>
    <object class="GtkLabel">
      <property name="visible">True</property>
      <property name="can_focus">False</property>
      <property name="label">${this.label}</property>
      <property name="justify">left</property>
      <property name="margin_left">5</property>
      <property name="margin_right">5</property>
      <property name="margin_top">5</property>
      <property name="margin_bottom">5</property>
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
      <property name="can_focus">True</property>
      <property name="max_length">${this.edit_limit}</property>
      <property name="width_chars">${this.edit_width}</property>
      <property name="text">${this.value}</property>
      <property name="margin_left">5</property>
      <property name="margin_right">5</property>
      <property name="margin_top">5</property>
      <property name="margin_bottom">5</property>
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

class RadioRow extends Cluster {
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

    toGtkXml() {
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(tile => this._child(tile.toGtkXml(group))).join('\n');
        return `
<object class="GtkBox">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">horizontal</property>
  <property name="spacing">2</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>
  <property name="valign">${this._ver_align(this.alignment)}</property>
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

class RadioColumn extends Cluster {
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

    toGtkXml() {
        let group = '';
        for (let tile of this._tiles) {
            if (tile.key) {
                group = tile.key;
                break;
            }
        }
        const tiles = this._tiles.map(tile => this._child(tile.toGtkXml(group))).join('\n');
        return `
<object class="GtkBox">
  <property name="visible">True</property>
  <property name="can_focus">False</property>
  <property name="orientation">vertical</property>
  <property name="spacing">2</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>

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
        //this.is_enabled = true;
        //this.is_tab_stop = true;
        this.key = null;
        this.label = '';
        //this.mnemonic = '';
        this.value = '';
        this.width = -1;
    }

    toGtkXml(radio) {
        const id = this.key ? `id="${this.key}"` : '';
        return `
<object class="GtkRadioButton" ${id}>
  <property name="label">${this.label}</property>
  <property name="visible">True</property>
  <property name="can_focus">True</property>
  <property name="receives_default">False</property>
  <property name="draw_indicator">True</property>
  <property name="group">${radio}</property>
  <property name="width_request">${this.width}</property>
  <property name="height_request">${this.height}</property>

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

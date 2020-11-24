class Tile {
    constructor(id) {
        this.id = id;
        this.key = null;
        this.label = '';
        this.value = '';
        this.width = -1;
        this.height = -1;
        this.alignment = 'centered';
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
        this.tiles = [];
    }

    addTile(tile) {
        this.tiles.push(tile);
    }

    getActions() {
        let list = [];
        for (let tile of this.tiles) {
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
        delete this.key;
        delete this.alignment;
    }

    toGtkXml() {
        const id = this.id ? `id="${this.id}"` : '';
        const tiles = this.tiles.map(tile => this._child(tile.toGtkXml())).join('\n');
        return `
  <object class="GtkWindow" ${id}>
    <property name="can_focus">False</property>
    <property name="title">${this.label}</property>
    <child>
      <object class="GtkBox">
        <property name="visible">True</property>
        <property name="can_focus">False</property>
        <property name="orientation">vertical</property>
        <property name="spacing">2</property>
        ${tiles}
      </object>
    </child>
  </object>
`;
    }
}

class Row extends Cluster {
    constructor() {
        super();
    }

    toGtkXml() {
        const tiles = this.tiles.map(tile => this._child(tile.toGtkXml())).join('\n');
        return `
          <object class="GtkBox">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="orientation">horizontal</property>
            <property name="spacing">2</property>
            <property name="homogeneous">True</property> <!-- children_fixed_width? -->
            ${tiles}
          </object>
`;
    }
}

class Column extends Cluster {
    constructor() {
        super();
    }

    toGtkXml() {
        const tiles = this.tiles.map(tile => this._child(tile.toGtkXml())).join('\n');
        return `
          <object class="GtkBox">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="orientation">vertical</property>
            <property name="spacing">2</property>
            ${tiles}
          </object>
`;
    }
}

class Spacer extends Tile {
    constructor() {
        super();
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
          </object>
`;
    }
}

class Text extends Tile {
    constructor(id) {
        super(id);
    }

    toGtkXml() {
        let id = '';
        if (this.id) {
            id = `id="${this.id}"`;
        } else if (this.key) {
            id = `id="${this.key}"`;
        }
        // TODO: also support this.value
        return `
          <object class="GtkLabel" ${id}>
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="label">${this.label}</property>
            <property name="width_request">${this.width}</property>
            <property name="height_request">${this.height}</property>
            <property name="halign">${this._hor_align(this.alignment)}</property>
          </object>
`;
    }
}

class Button extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.is_default = false;
    }

    toGtkXml() {
        let id = '';
        if (this.id) {
            id = `id="${this.id}"`;
        } else if (this.key) {
            id = `id="${this.key}"`;
        }
        return `
              <object class="GtkButton" ${id}>
                <property name="label">${this.label}</property>
                <property name="width_request">${this.width}</property>
                <property name="height_request">${this.height}</property>
                <property name="halign">${this._hor_align(this.alignment)}</property>
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="can_default">True</property>
                <property name="has_default">${this._bool(this.is_default)}</property>
                <property name="receives_default">True</property>
              </object>
              <packing>
                <property name="expand">True</property> <!-- fixed_width=false -->
                <property name="fill">True</property>
<!--                <property name="position">0</property> -->
              </packing>
`;
    }
}

class EditBox extends Tile {
    constructor(id) {
        super(id);
        this.action = '';
        this.is_default = false;
        this.edit_limit = 132;
        this.edit_width = 0;
    }

    toGtkXml() {
        let id = '';
        if (this.id) {
            id = `id="${this.id}"`;
        } else if (this.key) {
            id = `id="${this.key}"`;
        }
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
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
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
                  </object>
                  <packing>
                    <property name="expand">${this._bool(!this.edit_width > 0)}</property>
                    <property name="fill">True</property>
                    <property name="pack_type">end</property>
                    <property name="position">1</property>
                  </packing>
                </child>
              </object>
`;
    }
}

exports.Dialog = Dialog;
exports.Row = Row;
exports.Column = Column;
exports.Spacer = Spacer;
exports.Text = Text;
exports.Button = Button;
exports.EditBox = EditBox;

class Control {
    constructor(id) {
        this.id = id;
        this.key = null;
        this.label = '';
        this.value = '';
        this.alignment = 'centered';
        this.action = null;
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

class TileCluster extends Control {
    constructor(id) {
        super(id);
        this.controls = [];
    }

    addControl(control) {
        this.controls.push(control);
    }
}

class Dialog extends TileCluster {
    constructor(id) {
        super(id);
        delete this.key;
        delete this.action;
        delete this.alignment;
    }

    toGtkXml() {
        const id = this.id ? `id="${this.id}"` : '';
        const controls = this.controls.map(c => this._child(c.toGtkXml())).join('\n');
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
        <!-- BEGIN CONTROLS -->
        ${controls}
        <!-- END CONTROLS -->
      </object>
    </child>
  </object>
`;
    }
}

class Row extends TileCluster {
    constructor() {
        super();
    }

    toGtkXml() {
        const controls = this.controls.map(c => this._child(c.toGtkXml())).join('\n');
        return `
          <object class="GtkBox">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="orientation">horizontal</property>
            <property name="spacing">2</property>
            <property name="homogeneous">True</property> <!-- children_fixed_width? -->
            <!-- BEGIN CONTROLS -->
            ${controls}
            <!-- END CONTROLS -->
          </object>
`;
    }
}

class Column extends TileCluster {
    constructor() {
        super();
    }

    toGtkXml() {
        const controls = this.controls.map(c => this._child(c.toGtkXml())).join('\n');
        return `
          <object class="GtkBox">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="orientation">vertical</property>
            <property name="spacing">2</property>
            <!-- BEGIN CONTROLS -->
            ${controls}
            <!-- END CONTROLS -->
          </object>
`;
    }
}

class Text extends Control {
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
            <property name="halign">${this._hor_align(this.alignment)}</property>
          </object>
`;
    }
}

class Button extends Control {
    constructor(id) {
        super(id);
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
                <property name="halign">${this._hor_align(this.alignment)}</property>
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="can_default">True</property>
                <property name="has_default">${this._bool(this.is_default)}</property>
                <property name="receives_default">True</property>
              </object>
              <packing>
                <property name="expand">True</property>
                <property name="fill">True</property>
<!--                <property name="position">0</property> -->
              </packing>
`;
    }
}

class EditBox extends Control {
    constructor(id) {
        super(id);
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
                    <signal name="changed" handler="on_changed"/>
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
exports.Text = Text;
exports.Button = Button;
exports.EditBox = EditBox;

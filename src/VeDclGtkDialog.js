class VeDclGtkDialog {
    constructor(dialog) {
        this.dialog = dialog;
    }

    dialogXml() {
        return `
  <object class="GtkDialog" id="${this.dialog.id}">
    <property name="can_focus">False</property>
    <property name="type_hint">dialog</property>
    <child>
      <placeholder/>
    </child>
    <child internal-child="vbox">
      <object class="GtkBox">
        <property name="can_focus">False</property>
        <property name="orientation">vertical</property>
        <property name="spacing">2</property>
        <child internal-child="action_area">
          <object class="GtkButtonBox">
            <property name="can_focus">False</property>
            <property name="layout_style">end</property>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">False</property>
            <property name="position">0</property>
          </packing>
        </child>
        <child>
          <placeholder/>
        </child>
      </object>
    </child>
  </object>
`;
    }

    toString() {
        return `
<?xml version="1.0" encoding="UTF-8"?>
<interface>
  <requires lib="gtk+" version="3.20"/>
  ${this.dialogXml()}
</interface>
`;
    }
}

exports.VeDclGtkDialog = VeDclGtkDialog;

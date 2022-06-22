mnemonic_dlg : dialog {
  label = "Mnemonic Example";
  initial_focus = "accept";
  : row {
    : column {
      : edit_box {
        key = "edit1";
        label = "&Edit Box";
        mnemonic = "e"; // ignored
      }
      : list_box {
        key = "listbox1";
        label = "&List Box";
      }
      : popup_list {
        key = "popup1";
        label = "&Popup List";
      }
      : slider {
        key = "slider1";
        label = "&Slider";
      }
      : image_button {
        key = "imagebutton1";
        color = "red";
        mnemonic = "m";
        width = 5;
        height = 3;
        alignment = centered;
      }
    }

    : column {
      : edit_box {
        key = "edit2";
        label = "Edit Box";
        mnemonic = "e";
      }
      : list_box {
        key = "listbox2";
        label = "List Box";
        mnemonic = "l";
      }
      : popup_list {
        key = "popup2";
        label = "Popup List";
        mnemonic = "p";
      }
      : slider {
        key = "slider2";
        label = "Slider";
        mnemonic = "u";
      }
      : image_button {
        key = "imagebutton2";
        color = "green";
        mnemonic = "n";
        width = 5;
        height = 3;
        alignment = centered;
      }
    }
  }

  : row {
    : column {
      : toggle {
        key = "toggle1";
        label = "&Toggle";
      }
    }
    : column {
      : toggle {
        key = "toggle2";
        label = "Toggle";
        mnemonic = "g";
      }
    }
  }

  : radio_row {
    key = "radio_group";
    : radio_button {
      label = "&Radio Button";
      key = "radio1";
      width = 20;
    }
    : radio_button {
      label = "Radio Button";
      key = "radio2";
      width = 20;
      mnemonic = "a";
    }
  }

  ok_only;
}

mnemonic_dlg : dialog {
  label = "Mnemonic Example";
  initial_focus = "accept";
  : row {
    : edit_box {
      key = "edit1";
      label = "&Edit Box";
      edit_width = 10;
      mnemonic = "e"; // ignored
    }
    : edit_box {
      key = "edit2";
      label = "Edit Box";
      edit_width = 10;
      mnemonic = "e";
    }
  }

  : row {
    : list_box {
      key = "listbox1";
      label = "&List Box";
      width = 20;
    }
    : list_box {
      key = "listbox2";
      label = "List Box";
      width = 20;
      mnemonic = "l";
    }
  }

  : row {
    : popup_list {
      key = "popup1";
      label = "&Popup List";
      width = 20;
    }
    : popup_list {
      key = "popup2";
      label = "Popup List";
      width = 20;
      mnemonic = "p";
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

  : row {
    : toggle {
      key = "toggle1";
      label = "&Toggle";
      width = 20;
    }
    : toggle {
      key = "toggle2";
      label = "Toggle";
      width = 20;
      mnemonic = "g";
    }
  }

  : row {
    : slider {
      key = "slider1";
      label = "&Slider";
    }
    : slider {
      key = "slider2";
      label = "Slider";
      mnemonic = "u";
    }
  }

  : row {
    : column {
      : image_button {
        key = "imagebutton1";
        color = "red";
        mnemonic = "m";
        width = 5;
        height = 3;
      }
    }
    : column {
      : image_button {
        key = "imagebutton2";
        color = "green";
        mnemonic = "n";
        width = 5;
        height = 3;
      }
    }
  }

  ok_only;
}

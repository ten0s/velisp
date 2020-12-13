mnemonic_dlg : dialog {
  label = "Mnemonic Example";
  initial_focus = "accept";
  : row {
    : edit_box {
      key = "edit1";
      label = "_Edit Box";
      mnemonic = "e"; // ignored
    }
    : edit_box {
      key = "edit2";
      label = "Edit Box";
      mnemonic = "e";
    }
  }
  : row {
    : list_box {
      key = "listbox1";
      label = "_List Box";
    }
    : list_box {
      key = "listbox2";
      label = "List Box";
      mnemonic = "l";
    }
  }
  : row {
    : popup_list {
      key = "popup1";
      label = "_Popup List";
    }
    : popup_list {
      key = "popup2";
      label = "Popup List";
      mnemonic = "p";
    }
  }
  : radio_row {
    key = "radio_group";
    : radio_button {
      label = "_Radio Button";
      key = "radio1";
    }
    : radio_button {
      label = "Radio Button";
      key = "radio2";
      mnemonic = "a";
    }
  }
  : row {
    : toggle {
      key = "toggle1";
      label = "_Toggle";
    }
    : toggle {
      key = "toggle2";
      label = "Toggle";
      mnemonic = "g";
    }
  }
  : row {
    : slider {
      key = "slider1";
      label = "_Slider";
      width = 10;
    }
    : slider {
      key = "slider2";
      label = "Slider";
      width = 10;
      mnemonic = "u";
    }
  }
  : row {
    : image_button {
      key = "imagebutton1";
      color = "red";
      mnemonic = "m";
      width = 5;
      height = 3;
    }
    : image_button {
      key = "imagebutton2";
      color = "green";
      mnemonic = "n";
      width = 5;
      height = 3;
    }
  }
  ok_only;
}

dlg_1 : dialog {
  label = "Dialog 1";
  : text {
    label = "Text 1";
  }
  : text {
    label = "Text 2";
  }
  : button {
    key = "accept";
    label = "OK";
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    is_default = false;
  }
}

dlg_2 : dialog {
  label = "Dialog 2";
  : edit_box {
    key = "name";
    label = "Enter Your Name :";
    mnemonic = "N";
    alignment = centered;
    edit_limit = 30;
    edit_width = 30;
  }
  : text {
    label = "Text 1";
  }
  : button {
    key = "accept";
    label = "OK";
    is_default = true;
  }
}

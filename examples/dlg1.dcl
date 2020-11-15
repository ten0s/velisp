dlg_1 : dialog {
  label = "Dialog 1";
  : text {
    label = "Left";
    alignment = left;
  }
  : text {
    label = "Center";
    alignment = centered;
  }
  : text {
    label = "Right";
    alignment = right;
  }
  : row {
    : button {
      key = "accept";
      label = "OK";
      alignment = left;
      is_default = true;
    }
    : button {
      key = "cancel";
      label = "Cancel";
      alignment = centered;
      is_default = false;
    }
    : button {
      key = "help";
      label = "Help";
      alignment = right;
      is_default = false;
    }
  }
}

dlg_2 : dialog {
  label = "Dialog 2";
  : edit_box {
    key = "edit1";
    label = "Enter text:";
    value = "Change me";
    edit_limit = 10;
    edit_width = 10;
  }
  : edit_box {
    key = "edit2";
    label = "Enter text:";
    value = "Change me too";
    edit_limit = 15;
    edit_width = 15;
  }
  : text {
    key = "text1";
    label = "Change me";
  }
  : text {
    key = "text2";
    label = "Change me too";
  }
  : row {
    : button {
      key = "accept";
      label = "Concat";
      is_default = true;
    }
    : button {
      key = "cancel";
      label = "Cancel";
    }
  }
}

dlg_3 : dialog {
  label = "Dialog 3";
  : edit_box {
    key = "edit1";
    label = "Enter text:";
    action = "(set_tile \"text1\" $value)";
    edit_limit = 5;
    edit_width = 5;
  }
  : row {
    : button {
      key = "toggle";
      label = "Disable";
    }
    : button {
      key = "cancel";
      label = "Cancel";
    }
  }
}

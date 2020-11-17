dlg2 : dialog {
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

dlg3 : dialog {
  label = "Dialog 3";
  : edit_box {
    key = "edit1";
    label = "Enter text:";
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

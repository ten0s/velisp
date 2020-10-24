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
    key = "edit_id";
    label = "Enter Text: ";
    edit_limit = 30;
    edit_width = 30;
  }
  : text {
    key = "text_id"
    label = "Text";
  }
  : button {
    key = "accept";
    label = "OK";
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
  }
}

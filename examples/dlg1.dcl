dlg_1 : dialog {
  label = "Title";
  : text {
    label = "Text 1";
  }
  : text {
    label = "Text 2";
  }
  : button {
    key = "btn_ok";
    label = "OK";
    is_default = true;
  }
  : button {
    key = "btn_cancel";
    label = "Cancel";
    is_default = false;
  }
}

dlg_2 : dialog {
  label = "Title";
  : text {
    label = "Text 1";
  }
  : button {
    key = "btn_ok";
    label = "OK";
    is_default = true;
  }
}

get_attr_dlg : dialog {
  key = "get_attr_dialog";
  label = "Get Attr Example";
  : popup_list {
    key = "key_name";
    label = "Enter Key:";
    edit_width = 10;
  }
  : popup_list {
    key = "attr_name";
    label = "Enter Attr:";
    edit_width = 10;
  }
  : concatenation {
    : text_part {
      label = "Attr Value:";
    }
    : text_part {
      key = "attr_value";
      value = "";
    }
  }
  : row {
    : button {
      key = "get_attr";
      label = "Get Attr";
    }
    cancel_button;
  }
}

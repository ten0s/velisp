boxed_radio_column_dlg : dialog {
  label = "Boxed Radio Column";
  : row {
    alignment = centered;
    : text {
      label = "Current: ";
      is_bold = true;
    }
    : text {
      key = "current";
      value = "";
    }
  }
  : boxed_radio_column {
    key = "radio_group";
    label = "Boxed Radio Column";
    alignment = centered;
    : radio_button {
      label = "radio #1";
      key = "radio1";
      action = "(set_tile \"current\" $key)";
    }
    : radio_button {
      label = "radio #2";
      key = "radio2";
      value = "1";
      action = "(set_tile \"current\" $key)";
    }
    : radio_button {
      label = "radio #3";
      key = "radio3";
      action = "(set_tile \"current\" $key)";
    }
    : radio_button {
      label = "radio #4";
      key = "radio4";
      action = "(set_tile \"current\" $key)";
      is_enabled = false;
    }
  }
  ok_only;
}

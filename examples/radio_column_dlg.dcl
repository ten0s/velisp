radio_column_dlg : dialog {
  label = "Radio Column";
  : radio_column {
    : radio_button {
      label = "radio #1";
      key = "radio1";
      action = "(princ $key)";
    }
    : radio_button{
      label = "radio #2";
      key = "radio2";
      action = "(princ $key)";
    }
    : radio_button{
      label = "radio #3";
      key = "radio3";
      action = "(princ $key)";
    }
  }
  ok_only;
}

boxed_radio_row_dlg : dialog {
  label = "Boxed Radio Row";
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
  : boxed_radio_row {
    key = "radio_group";
    label = "Boxed Radio Row";
    : radio_button {
        label = "radio #1";
        key = "radio1";
    }
    : radio_button {
      label = "radio #2";
      key = "radio2";
    }
    : radio_button {
      label = "radio #3";
      key = "radio3";
    }
    : radio_button {
      label = "radio #4";
      key = "radio4";
    }
  }
  ok_only;
}

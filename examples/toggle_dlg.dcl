toggle_dlg : dialog {
  label = "Toggle";
  : boxed_row {
    label = "Boxed Row";
    : boxed_column {
      label = "1";
      : text {
        key = "toggle1-value";
        value = "";
      }
      : toggle {
        key = "toggle1";
        label = "toggle #1";
        value = "1";
        action = "(set_tile \"toggle1-value\" (if (= $value \"1\") \"on\" \"off\"))";
      }
    }
    : boxed_column {
      label = "2";
      : text {
        key = "toggle2-value";
        value = "";
      }
      : toggle {
        key = "toggle2";
        label = "toggle #2";
      }
    }
  }
  ok_only;
}

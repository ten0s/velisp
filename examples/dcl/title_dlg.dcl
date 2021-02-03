title_dlg : dialog {
  key = "dialog";
  value = "";
  : row {
    alignment = centered;
    : text {
      label = "Old Title: ";
      is_bold = true;
    }
    : text {
      key = "old_title";
      value = "";
    }
  }
  : row {
    alignment = centered;
    : button {
      key = "change";
      label = "Change Title";
      is_default = true;
    }
    : button {
      key = "cancel";
      label = "Cancel";
    }
  }
}

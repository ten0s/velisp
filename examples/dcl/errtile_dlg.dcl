errtile_dlg : dialog {
  label = "ErrTile Example";
  : edit_box {
    key = "edit";
    label = "Enter integer:";
    edit_width = 5;
  }
  : text {
    key = "text";
    value = "";
    alignment = centered;
  }
  : row {
    alignment = centered;
    : button {
      key = "getint";
      label = "Get Integer";
    }
    : button {
      key = "cancel";
      label = "Cancel";
    }
  }
  errtile;
}

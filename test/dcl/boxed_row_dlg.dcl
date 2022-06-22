boxed_row_dlg : dialog {
  label = "Boxed Row";
  : boxed_row {
    label = "Plot Origin";
    : edit_box {
      key = "x_orig";
      label = "X Origin: ";
      value = "0.00";
      edit_width = 7;
    }
    : edit_box {
      key = "y_orig";
      label = "Y Origin: ";
      value = "0.00";
      edit_width = 7;
    }
  }
  ok_only;
}

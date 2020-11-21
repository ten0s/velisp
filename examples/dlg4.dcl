@include "lib/dcl/base.dcl"

dlg4 : dialog {
  label = "Dialog 4";
  : edit_box {
    key = "edit";
    label = "Enter integer:";
    edit_width = 5;
  }
  : text {
    key = "text";
    value = "";
  }
  : row {
    : button {
      key = "getint";
      label = "Get Integer";
    }
    : button {
      key = "cancel";
      label = "Cancel";
    }
  }
  : errtile { }
  // errtile;
}

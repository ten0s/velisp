dlg1 : dialog {
  label = "Dialog 1";
  : text {
    label = "Left";
    alignment = left;
  }
  : text {
    label = "Center";
    alignment = centered;
  }
  : text {
    label = "Right";
    alignment = right;
  }
  : row {
    : button {
      key = "accept";
      label = "OK";
      alignment = left;
      is_default = true;
    }
    : button {
      key = "cancel";
      label = "Cancel";
      alignment = centered;
      is_default = false;
    }
    : button {
      key = "help";
      label = "Help";
      alignment = right;
      is_default = false;
    }
  }
}

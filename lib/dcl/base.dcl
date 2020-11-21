//
// Basic tile definitions
//

errtile : text {
  key = "error";
  alignment = left;
}

ok_only : row {
  : button {
    key = "accept";
    label = "OK";
    action = "(done_dialog)";
    alignment = centered;
    is_default = true;
  }
}

on_cancel : row {
  : button {
    key = "accept";
    label = "OK";
    action = "(done_dialog)";
    alignment = left;
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    action = "(done_dialog)";
    alignment = right;
    is_default = false;
  }
}

on_cancel_help : row {
  : button {
    key = "accept";
    label = "OK";
    action = "(done_dialog)";
    alignment = left;
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    action = "(done_dialog)";
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

on_cancel_help_info : row {
  : button {
    key = "accept";
    label = "OK";
    action = "(done_dialog)";
    alignment = left;
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    action = "(done_dialog)";
    alignment = centered;
    is_default = false;
  }
  : button {
    key = "help";
    label = "Help";
    alignment = right;
    is_default = false;
  }
  : button {
    key = "info";
    label = "Info";
    alignment = right;
    is_default = false;
  }
}

on_cancel_help_errtile : column {
  : row {
    : button {
      key = "accept";
      label = "OK";
      action = "(done_dialog)";
      alignment = left;
      is_default = true;
    }
    : button {
      key = "cancel";
      label = "Cancel";
      action = "(done_dialog)";
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
  errtile;
}

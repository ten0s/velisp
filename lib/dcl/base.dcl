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
    action = "(done_dialog 1)";
    is_default = true;
  }
}

on_cancel : row {
  : button {
    key = "accept";
    label = "OK";
    action = "(done_dialog 1)";
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    action = "(done_dialog 0)";
    is_default = false;
  }
}

on_cancel_help : row {
  : button {
    key = "accept";
    label = "OK";
    action = "(done_dialog 1)";
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    action = "(done_dialog 0)";
    is_default = false;
  }
  : button {
    key = "help";
    label = "Help";
    is_default = false;
  }
}

on_cancel_help_info : row {
  : button {
    key = "accept";
    label = "OK";
    action = "(done_dialog 1)";
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    action = "(done_dialog 0)";
    is_default = false;
  }
  : button {
    key = "help";
    label = "Help";
    is_default = false;
  }
  : button {
    key = "info";
    label = "Info...";
    is_default = false;
  }
}

on_cancel_help_errtile : column {
  alignment = filled;
  : row {
    : button {
      key = "accept";
      label = "OK";
      action = "(done_dialog 1)";
      is_default = true;
    }
    : button {
      key = "cancel";
      label = "Cancel";
      action = "(done_dialog 0)";
      is_default = false;
    }
    : button {
      key = "help";
      label = "Help";
      is_default = false;
    }
  }
  : row {
    errtile;
  }
}

spacer_0 : spacer {
  width = 0;
  height = 0;
}

spacer_1 : spacer {
  width = 1;
  height = 1;
}

//
// Basic tile definitions
//

ok_button : button {
  key = "accept";
  label = "_OK";
  action = "(done_dialog 1)";
}

cancel_button : button {
  key = "cancel";
  label = "_Cancel";
  action = "(done_dialog 0)";
}

help_button : button {
  key = "help";
  label = "_Help";
}

info_button : button {
  key = "info";
  label = "_Info...";
}

errtile : text {
  key = "error";
  alignment = left;
}

ok_only : column {
  alignment = centered;
  : row {
    : ok_button {
      is_default = true;
      width = 8;
    }
  }
}

ok_cancel : column {
  alignment = centered;
  : row {
    : ok_button {
      is_default = true;
      width = 8;
    }
    : cancel_button {
      width = 8;
    }
  }
}

ok_cancel_help : column {
  alignment = centered;
  : row {
    : ok_button {
      is_default = true;
      width = 8;
    }
    : cancel_button {
      width = 8;
    }
    : help_button {
      width = 8;
    }
  }
}

ok_cancel_help_info : column {
  alignment = centered;
  : row {
    : ok_button {
      is_default = true;
      width = 8;
    }
    : cancel_button {
      width = 8;
    }
    : help_button {
      width = 8;
    }
    : info_button {
      width = 8;
    }
  }
}

ok_cancel_help_errtile : column {
  alignment = filled;
  : row {
    alignment = centered;
    : ok_button {
      is_default = true;
      width = 8;
    }
    : cancel_button {
      width = 8;
    }
    : help_button {
      width = 8;
    }
  }
  : row {
    alignment = left;
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

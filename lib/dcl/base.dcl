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

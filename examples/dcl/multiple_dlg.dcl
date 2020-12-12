multiple_dlg : dialog {
  key = "dialog";
  value = "";
  : row {
    alignment = centered;
    : button {
      key = "new_dlg";
      label = "New Dialog";
      is_default = true;
    }
    : button {
      key = "term_all";
      label = "Term All";
    }
  }
  ok_cancel;
}

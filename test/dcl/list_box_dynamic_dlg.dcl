list_box_dynamic_dlg : dialog {
  label = "List Box Dynamic";
  : row {
    alignment = centered;
    : text {
      label = "Value: ";
    }
    : text {
      key = "listbox1-value";
      value = "";
    }
  }
  : list_box {
    key = "listbox1";
    list = "One\nTwo\nThree";
  }
  spacer_1;

  : row {
    alignment = centered;
    : text {
      label = "Value: ";
    }
    : text {
      key = "listbox2-value";
      value = "";
    }
  }
  : list_box {
    key = "listbox2";
    list = "One\nTwo\nThree";
  }
  spacer_1;

  : row {
    alignment = centered;
    : text {
      label = "Value: ";
    }
    : text {
      key = "listbox3-value";
      value = "";
    }
  }
  : list_box {
    key = "listbox3";
    list = "One\nTwo";
  }
  spacer_1;

  ok_only;
}

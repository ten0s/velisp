list_box_dlg : dialog {
  label = "List Box";
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
    list = "North\nSouth\nEast\nWest";
    action = "(set_tile \"listbox1-value\" $value)";
    value = ""; // TODO: doesn't work for single
  }
  spacer;

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
    label = "Week Days";
    list = "Monday\nTuesday\nWednesday\nThursday\nFriday\nSaturday\nSunday";
    multiple_select = true;
    value = "0 3";
  }
  spacer;

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
    label = "Months";
    list = "Jan\nFeb\nMar\nApr\nMay\nJun\nJul\nAug\nSep\nOct\nNov\nDec";
    multiple_select = true;
  }
  spacer;

  ok_only;
}

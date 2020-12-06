popup_list_dlg : dialog {
  label = "Popup List";
  : row {
    alignment = centered;
    : text {
      label = "Value: ";
    }
    : text {
      key = "popup1-value";
      value = "";
    }
  }
  : popup_list {
    key = "popup1";
    list = "North\nSouth\nEast\nWest";
    value = "1";
    action = "(set_tile \"popup1-value\" $value)";
  }
  spacer_1;

  : row {
    alignment = centered;
    : text {
      label = "Value: ";
    }
    : text {
      key = "popup2-value";
      value = "";
    }
  }
  : popup_list {
    key = "popup2";
    label = "Select: ";
    list = "Monday\nTuesday\nWednesday\nThursday\nFriday\nSaturday\nSunday";
  }
  spacer_1;

  : row {
    alignment = centered;
    : text {
      label = "Value: ";
    }
    : text {
      key = "popup3-value";
      value = "";
    }
  }
  : popup_list {
    key = "popup3";
    label = "Select: ";
    edit_width = "5";
    list = "Jan\nFeb\nMar\nApr\nMay\nJun\nJul\nAug\nSep\nOct\nNov\nDec";
  }
  spacer_1;

  ok_only;
}

popup_list_dynamic_dlg : dialog {
  label = "Popup List Dynamic";
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
    list = "One\nTwo\nThree";
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
    list = "One\nTwo\nThree";
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
    list = "One\nTwo";
  }
  spacer_1;
  
  ok_only;
}

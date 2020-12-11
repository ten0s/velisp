tab_stop_dlg : dialog {
  label = "Tab Stop Example";
  initial_focus = "accept";
  : column {
    : button {
      key = "button";
      label = "Button";
      is_tab_stop = false;
    }
    : edit_box {
      key = "edit";
      is_tab_stop = false;
    }
    : popup_list {
      key = "popup";
      // TODO: make it work somehow
      is_tab_stop = false;
    }
    : list_box {
      key = "listbox";
      is_tab_stop = false;
    }
 }
  ok_cancel;
}

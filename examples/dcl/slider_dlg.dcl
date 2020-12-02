slider_dlg : dialog {
  label = "Slider";
  : boxed_row {
    label = "Horizontal";
    : column {
      : text {
        key = "slider1-value";
        value = "";
      }
      : slider {
        key = "slider1";
        action = "(set_tile \"slider1-value\" $value)";
        //height = 5;
        width = 30;
      }
    }
  }
  : boxed_row {
    label = "Vertical";
    : column {
      : text {
        key = "slider2-value";
        value = "";
      }
      : slider {
        key = "slider2";
        min_value = 0;
        max_value = 100;
        value = 50;
        layout = vertical;
        height = 10;
        width = 30;
      }
    }
  }
  ok_only;
}

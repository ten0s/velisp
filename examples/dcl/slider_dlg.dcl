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
        //width = 30; // Width is set automatically
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
        // Height should be specified explicitely, since
        // there's no other elements to calculate it
        height = 10;
      }
    }
  }
  ok_only;
}

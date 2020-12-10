image_button_dlg : dialog {
  label = "Image Button";
  : image_button {
    alignment = centered;
    key = "button";
    width = 4;
    height = 2;
  }
  : concatenation {
    alignment = centered;
    : text_part {
      label = "Clicked: ";
    }
    : text_part {
      key = "counter";
      value = "0";
    }
    : text_part {
      label = " times";
    }
    : text_part {
      label = ", at: ";
    }
    : text_part {
      key = "coords";
      value = "";
    }
  }
}

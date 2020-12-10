image_button_dlg : dialog {
  label = "Image Button";
  : image_button {
    alignment = centered;
    key = "imagebutton1";
    width = 4;
    height = 2;
  }
  : concatenation {
    alignment = centered;
    : text_part {
      label = "Clicked: ";               
    }
    : text_part {
      key = "text1";
      value = "0";
    }
  }
}

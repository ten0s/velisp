text_part_dlg : dialog {
  label = "Text Part";
  : paragraph {
    alignment = centered;
    : concatenation {
      : text_part  {
        label = "One";
      }
      : text_part {
        label = "good";
        is_bold = true;
      }
      : text_part {
        label = "turn";
      }
    }
    : text_part {
      label = "Deserves another";
    }
  }
}

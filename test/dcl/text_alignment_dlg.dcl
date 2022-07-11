// Column by default
//
// Left
//       Center
//               Right
//       Center
// Left

text_alignment_dlg : dialog {
  label = "Text Alignment";
  : text {
    label = "Left";
    alignment = left;
  }
  : text {
    label = "Center";
    alignment = centered;
  }
  : text {
    label = "Right";
    alignment = right;
  }
  : text {
    label = "Center";
    alignment = centered;
  }
  : text {
    label = "Left";
    alignment = left;
  }

  ok_only;
}

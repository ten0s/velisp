row_alignment_dlg : dialog {
  label = "Row Alignment";

  : text { label = "No Column"; }
  : row {
    height = 8;
    : button {
      label = "Top";
      width = 10;
      alignment = top;
    }
    : button {
      label = "Center";
      width = 10;
      alignment = centered;
    }
    : button {
      label = "Bottom";
      width = 10;
      alignment = bottom;
    }
  }

  : text { label = "Column"; }
  : column {
    : row {
      height = 8;
      : button {
        label = "Top";
        width = 10;
        alignment = top;
      }
      : button {
        label = "Center";
        width = 10;
        alignment = centered;
      }
      : button {
        label = "Bottom";
        width = 10;
        alignment = bottom;
      }
    }
  }

  : boxed_row {
    label = "Boxed Row";
    height = 8;
    : button {
      label = "Top";
      width = 10;
      alignment = top;
    }
    : button {
      label = "Center";
      width = 10;
      alignment = centered;
    }
    : button {
      label = "Bottom";
      width = 10;
      alignment = bottom;
    }
  }
  
  : boxed_radio_row {
    label = "Boxed Radio Row";
    height = 8;
    : button {
      label = "Top";
      width = 10;
      alignment = top;
    }
    : button {
      label = "Center";
      width = 10;
      alignment = centered;
    }
    : button {
      label = "Bottom";
      width = 10;
      alignment = bottom;
    }
  }
  
  ok_only;
}

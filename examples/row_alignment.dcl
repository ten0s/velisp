row_alignment : dialog {
  label = "Row Alignment";

  : text { label = "No Column"; }
  : row {
    alignment = filled;
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
      alignment = filled;
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
    alignment = filled;
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
    alignment = filled;
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

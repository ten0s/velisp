column_alignment_dlg : dialog {
  label = "Column Alignment";

  : text { label = "No Column / No Row wrap"; }
  : button {
    label = "Left";
    width = 10;
    alignment = left;
  }
  : button {
    label = "Center";
    width = 10;
    alignment = centered;
  }
  : button {
    label = "Right";
    width = 10;
    alignment = right;
  }

  : text { label = "No Column / Row wrap"; }
  : row {
    alignment = left;
    : button {
      label = "Left";
      width = 10;
    }
  }
  : row {
    alignment = centered;
    : button {
      label = "Center";
      width = 10;
    }
  }
  : row {
    alignment = right;
    : button {
      label = "Right";
      width = 10;
    }
  }

  : text { label = "Column / No Row wrap"; }
  : column {
    : button {
      label = "Left";
      width = 10;
      alignment = left;
    }
    : button {
      label = "Center";
      width = 10;
      alignment = centered;
    }
    : button {
      label = "Right";
      width = 10;
      alignment = right;
    }
  }

  : text { label = "Column / Row wrap"; }
  : column {
    : row {
      alignment = left;
      : button {
        label = "Left";
        width = 10;
      }
    }
    : row {
      alignment = centered;
      : button {
        label = "Center";
        width = 10;
      }
    }
    : row {
      alignment = right;
      : button {
        label = "Right";
        width = 10;
      }
    }
  }

  : boxed_column {
    label = "Boxed Column / No Row wrap";
    : button {
      label = "Left";
      width = 10;
      alignment = left;
    }
    : button {
      label = "Center";
      width = 10;
      alignment = centered;
    }
    : button {
      label = "Right";
      width = 10;
      alignment = right;
    }
  }
  
  : boxed_radio_column {
    label = "Boxed Radio Column / Row wrap";
    //alignment = filled;
    : row {
      alignment = left;
      : button {
        label = "Left";
        width = 10;
      }
    }
    : row {
      alignment = centered;
      : button {
        label = "Center";
        width = 10;
      }
    }
    : row {
      alignment = right;
      : button {
        label = "Right";
        width = 10;
      }
    }
  }
  
  ok_only;
}

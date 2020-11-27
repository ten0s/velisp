mode_tile_dlg : dialog {
  label = "Mode Tile Example";
  : edit_box {
    key = "edit1";
    label = "Enter text:";
    is_enabled = false;
    edit_limit = 5;
    edit_width = 5;
  }
  : row {
    : button {
      key = "toggle";
      label = "Enable";
    }
    : button {
      key = "cancel";
      label = "Cancel";
    }
  }
}

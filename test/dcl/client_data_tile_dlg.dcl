client_data_tile_dlg : dialog {
  label = "Client Data Tile Example";
  initial_focus = "button";
  : row {
    : button {
      key = "button";
      label = "Click me!";
    }
    : concatenation {
      : text_part {
        label = "Client data: ";
      }
      : text_part {
        key = "data";
        value = "";
      }
    }
  }
  : row {
    alignment = centered;
    : button {
      key = "toggle";
      width = 10;
    }
    cancel_button;
  }
}

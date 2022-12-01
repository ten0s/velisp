// SPDX-License-Identifier: 0BSD

cell_button : image_button {
  width = 8;
  height = 4;
}

fifteen : dialog {
  label = "Fifteen Puzzle";
  : column {
    : row {
      : cell_button {
        key = "1x1";
      }
      : cell_button {
        key = "1x2";
      }
      : cell_button {
        key = "1x3";
      }
      : cell_button {
        key = "1x4";
      }
    }
    : row {
      : cell_button {
        key = "2x1";
      }
      : cell_button {
        key = "2x2";
      }
      : cell_button {
        key = "2x3";
      }
      : cell_button {
        key = "2x4";
      }
    }
    : row {
      : cell_button {
        key = "3x1";
      }
      : cell_button {
        key = "3x2";
      }
      : cell_button {
        key = "3x3";
      }
      : cell_button {
        key = "3x4";
      }
    }
    : row {
      : cell_button {
        key = "4x1";
      }
      : cell_button {
        key = "4x2";
      }
      : cell_button {
        key = "4x3";
      }
      : cell_button {
        key = "4x4";
      }
    }
  }
  : column {
    alignment = centered;
    : row {
      : button {
        label = "&New Game";
        key = "new_game";
      }
      : cancel_button {
        label = "E&xit";
        is_default = true;
      }
    }
  }
  errtile;
}

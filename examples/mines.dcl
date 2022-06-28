// SPDX-License-Identifier: 0BSD

mine_button : image_button {
  width = 4;
  height = 2;
}

mines : dialog {
  label = "Mines";
  : column {
    alignment = centered;
    : row {
      : image {
        key = "score_flag";
        width = 2;
        height = 3;
      }
      : text {
        key = "score_text";
        label = "";
        width = 10;
      }
      : slider {
        key = "difficulty";
        min_value = 1;
        max_value = 16;
        value = 8;
        width = 20;
      }
    }
  }
  : column {
    : row {
      : mine_button {
        key = "1x1";
      }
      : mine_button {
        key = "1x2";
      }
      : mine_button {
        key = "1x3";
      }
      : mine_button {
        key = "1x4";
      }
      : mine_button {
        key = "1x5";
      }
      : mine_button {
        key = "1x6";
      }
      : mine_button {
        key = "1x7";
      }
      : mine_button {
        key = "1x8";
      }
    }
    : row {
      : mine_button {
        key = "2x1";
      }
      : mine_button {
        key = "2x2";
      }
      : mine_button {
        key = "2x3";
      }
      : mine_button {
        key = "2x4";
      }
      : mine_button {
        key = "2x5";
      }
      : mine_button {
        key = "2x6";
      }
      : mine_button {
        key = "2x7";
      }
      : mine_button {
        key = "2x8";
      }
    }
    : row {
      : mine_button {
        key = "3x1";
      }
      : mine_button {
        key = "3x2";
      }
      : mine_button {
        key = "3x3";
      }
      : mine_button {
        key = "3x4";
      }
      : mine_button {
        key = "3x5";
      }
      : mine_button {
        key = "3x6";
      }
      : mine_button {
        key = "3x7";
      }
      : mine_button {
        key = "3x8";
      }
    }
    : row {
      : mine_button {
        key = "4x1";
      }
      : mine_button {
        key = "4x2";
      }
      : mine_button {
        key = "4x3";
      }
      : mine_button {
        key = "4x4";
      }
      : mine_button {
        key = "4x5";
      }
      : mine_button {
        key = "4x6";
      }
      : mine_button {
        key = "4x7";
      }
      : mine_button {
        key = "4x8";
      }
    }
    : row {
      : mine_button {
        key = "5x1";
      }
      : mine_button {
        key = "5x2";
      }
      : mine_button {
        key = "5x3";
      }
      : mine_button {
        key = "5x4";
      }
      : mine_button {
        key = "5x5";
      }
      : mine_button {
        key = "5x6";
      }
      : mine_button {
        key = "5x7";
      }
      : mine_button {
        key = "5x8";
      }
    }
    : row {
      : mine_button {
        key = "6x1";
      }
      : mine_button {
        key = "6x2";
      }
      : mine_button {
        key = "6x3";
      }
      : mine_button {
        key = "6x4";
      }
      : mine_button {
        key = "6x5";
      }
      : mine_button {
        key = "6x6";
      }
      : mine_button {
        key = "6x7";
      }
      : mine_button {
        key = "6x8";
      }
    }
    : row {
      : mine_button {
        key = "7x1";
      }
      : mine_button {
        key = "7x2";
      }
      : mine_button {
        key = "7x3";
      }
      : mine_button {
        key = "7x4";
      }
      : mine_button {
        key = "7x5";
      }
      : mine_button {
        key = "7x6";
      }
      : mine_button {
        key = "7x7";
      }
      : mine_button {
        key = "7x8";
      }
    }
    : row {
      : mine_button {
        key = "8x1";
      }
      : mine_button {
        key = "8x2";
      }
      : mine_button {
        key = "8x3";
      }
      : mine_button {
        key = "8x4";
      }
      : mine_button {
        key = "8x5";
      }
      : mine_button {
        key = "8x6";
      }
      : mine_button {
        key = "8x7";
      }
      : mine_button {
        key = "8x8";
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

// SPDX-License-Identifier: 0BSD

calc_button : button {
  width = 8;
  height = 4;
}

calc_dlg : dialog {
  label = "Calculator";
  : edit_box {
    key = "input";
    value = "0";
    justify = "right";
    height = 3;
    is_enabled = false;
  }
  : column {
    : row {
      : calc_button {
        label = "C";
        key = "clear";
      }
      : calc_button {
        label = "Back";
        key = "backspace";
      }
      : calc_button {
        label = "Sqrt";
        key = "sqrt";
      }
      : calc_button {
        label = "+/-";
        key = "negate";
      }
    }
    : row {
      : calc_button {
        label = "7";
        key = "seven";
      }
      : calc_button {
        label = "8";
        key = "eight";
      }
      : calc_button {
        label = "9";
        key = "nine";
      }
      : calc_button {
        label = "/";
        key = "divide";
      }
    }
    : row {
      : calc_button {
        label = "4";
        key = "four";
      }
      : calc_button {
        label = "5";
        key = "five";
      }
      : calc_button {
        label = "6";
        key = "six";
      }
      : calc_button {
        label = "*";
        key = "multiply";
      }
    }
    : row {
      : calc_button {
        label = "1";
        key = "one";
      }
      : calc_button {
        label = "2";
        key = "two";
      }
      : calc_button {
        label = "3";
        key = "three";
      }
      : calc_button {
        label = "-";
        key = "subtract";
      }
    }
    : row {
      : calc_button {
        label = "0";
        key = "zero";
      }
      : calc_button {
        label = ".";
        key = "period";
      }
      : calc_button {
        label = "=";
        key = "equal";
      }
      : calc_button {
        label = "+";
        key = "add";
      }
    }
  }
  : column {
    : cancel_button {
        label = "Exit";
        alignment = centered;
        is_default = true;
      }
  }
}

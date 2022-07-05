// SPDX-License-Identifier: 0BSD

demo_dlg : dialog {
  label = "DCL Demo";
  : row {
    : column {
      : text {
        label = "Names";
        alignment = centered;
      }
      : list_box {
        key = "listbox_names";
        width = 30;
        height = 43;
      }
    }
    : column {
      : row {
        : text {
          key = "text_dcl";
          label = "";
          alignment = centered;
        }
        : button {
          key = "button_dcl";
          label = "Open";
          alignment = right;
        }
      }
      : row {
        : list_box {
          key = "listbox_dcl";
          width = 100;
          height = 18;
        }
      }

      : row {
        : text {
          key = "text_lsp";
          label = "";
          alignment = right;
        }
        : button {
          key = "button_lsp";
          label = "Open";
        }
      }
      : row {
        : list_box {
          key = "listbox_lsp";
          width = 100;
          height = 18;
        }
      }
    }
  }

  : row {
    : column {
      : button {
        alignment = left;
        key = "button_run";
        label = "&Run";
        width = 10;
      }
    }

    : column {
      : button {
        alignment = right;
        key = "button_exit";
        label = "E&xit";
        width = 10;
      }
    }
  }
}

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
      : text {
        key = "text_dcl";
        label = "";
        alignment = centered;
      }
      : list_box {
        key = "listbox_dcl";
        width = 100;
        height = 20;
      }

      : text {
        key = "text_lsp";
        label = "";
        alignment = centered;
      }
      : list_box {
        key = "listbox_lsp";
        width = 100;
        height = 20;
      }
    }
  }

  : row {
    : column {
      : button {
        alignment = left;
        key = "run";
        label = "&Run";
        width = 10;
      }
    }

    : column {
      : button {
        alignment = right;
        key = "exit";
        label = "E&xit";
        width = 10;
      }
    }
  }
}

// SPDX-License-Identifier: 0BSD

slides_dlg : dialog {
  label = "Slides";
  : row {
    : column {
      : list_box {
        key = "names";
        width = 20;
        height = 20;
      }
    }
    : column {
      : image {
        key = "image";
        width = 50;
        height = 20;
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

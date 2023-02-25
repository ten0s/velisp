// SPDX-License-Identifier: 0BSD

slides_dlg : dialog {
  label = "Slides";
  : row {
    : column {
      : list_box {
        label = "Slide Names:";
        key = "names";
        width = 40;
        height = 38;
      }
    }
    : column {
      : row {
        : column {
          : text {
            label = "Slide Image:";
            alignment = centered;
          }
          : image {
            key = "image";
            width = 50;
            height = 25;
          }
        }
        : column {
          : list_box {
            label = "Slide Info:";
            key = "slide-info";
            width = 32;
            height = 14;
          }
          : list_box {
            label = "Library Info:";
            key = "lib-info";
            width = 41;
            height = 10;
          }
        }
      }
      : row {
        : list_box {
          label = "Slide Records:";
          key = "slide-recs";
          width = 92;
          height = 10;
        }
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

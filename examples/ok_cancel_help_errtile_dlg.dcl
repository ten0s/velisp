@include "lib/dcl/base.dcl"

ok_cancel_help_errtile_dlg : dialog {
  label = "OK/Cancel/Help/ErrTile";
  : text {
    label = "Hello";
    alignment = centered;
  }
  on_cancel_help_errtile;
}

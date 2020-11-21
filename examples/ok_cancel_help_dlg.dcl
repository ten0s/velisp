@include "lib/dcl/base.dcl"

ok_cancel_help_dlg : dialog {
  label = "OK/Cancel/Help";
  : text {
    label = "Hello";
    alignment = centered;
  }
  on_cancel_help;
}

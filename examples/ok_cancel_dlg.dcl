@include "lib/dcl/base.dcl"

ok_cancel_dlg : dialog {
  label = "OK/Cancel";
  : text {
    label = "Hello";
    alignment = centered;
  }
  on_cancel;
}

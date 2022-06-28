;;;; SPDX-License-Identifier: 0BSD

(if %VELISP_DCL%
    (foreach name '("alert" "consts")
             (load (strcat (getenv "VELISP_ROOT") "/lib/dcl/" name ".lsp"))))

;;;; SPDX-License-Identifier: 0BSD

(if %VELISP_DCL%
    (foreach name '("alert" "colors" "consts")
             (load (strcat (getenv "VELISP_ROOT") "/lib/dcl/" name ".lsp"))))

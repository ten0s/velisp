;;;; SPDX-License-Identifier: 0BSD

(foreach name '("base" "list" "vl-filename" "vl-list" "vl-string")
         (load (strcat (getenv "VELISP_ROOT") "/lib/kernel/" name "-test.lsp")))

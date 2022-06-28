;;;; SPDX-License-Identifier: 0BSD

(foreach name '("list" "random" "string")
         (load (strcat (getenv "VELISP_ROOT") "/lib/velisp/" name "-test.lsp")))

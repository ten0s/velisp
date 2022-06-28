;;;; SPDX-License-Identifier: 0BSD

(foreach name '("list" "random" "string" "system")
         (load (strcat (getenv "VELISP_ROOT") "/lib/velisp/" name ".lsp")))

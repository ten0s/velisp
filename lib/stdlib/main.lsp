(foreach name '("list" "system" "vl-filename" "vl-list" "vl-string" "ve-list" "ve-string")
         (load (strcat (getenv "VELISP_ROOT") "/lib/stdlib/" name ".lsp")))

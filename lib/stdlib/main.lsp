(foreach name '("list" "vl-filename" "vl-list" "vl-string" "ve-string")
         (load (strcat (getenv "VELISP_ROOT") "/lib/stdlib/" name ".lsp")))

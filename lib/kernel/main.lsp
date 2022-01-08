(foreach name '("base" "list" "vl-filename" "vl-list" "vl-string")
         (load (strcat (getenv "VELISP_ROOT") "/lib/kernel/" name ".lsp")))

(foreach name '("list" "random" "string" "system")
         (load (strcat (getenv "VELISP_ROOT") "/lib/velisp/" name ".lsp")))

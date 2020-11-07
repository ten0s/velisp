(foreach name '("kernel" "stdlib" "vl-unit")
         (load (strcat (getenv "VELISP_ROOT") "/lib/" name "/main.lsp")))

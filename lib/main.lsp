(foreach name '("kernel" "velisp" "vl-unit" "dcl")
         (load (strcat (getenv "VELISP_ROOT") "/lib/" name "/main.lsp")))

(vlu-remove-tests)

(foreach name '("kernel" "stdlib" "vl-unit")
         (load (strcat (getenv "VELISP_ROOT") "/lib/" name "/test.lsp")))

(vlu-run-tests)

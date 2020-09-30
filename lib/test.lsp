(vlu-remove-tests)

(load (strcat (getenv "VELISP_ROOT") "/lib/kernel/test.lsp"))
(load (strcat (getenv "VELISP_ROOT") "/lib/stdlib/test.lsp"))
(load (strcat (getenv "VELISP_ROOT") "/lib/vl-unit/test.lsp"))

(vlu-run-tests)

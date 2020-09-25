(vlu-remove-tests)

(load (strcat (getenv "VELISP_ROOT") "/lib/stdlib/list-test.lsp"))
(load (strcat (getenv "VELISP_ROOT") "/lib/stdlib/vl-list-test.lsp"))

(vlu-run-tests)

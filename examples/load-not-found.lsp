(princ (load "examples/not-found.lsp" "error-handled-1"))
(princ (load "examples/not-found.lsp" (defun onfail () 'error-handled-2)))
(load "examples/not-found.lsp")

; Example from https://en.wikipedia.org/wiki/Dialog_Control_Language

(defun change-name ()
  (set_tile "greeting" (strcat "Hello, " (get_tile "name") "!")))

(setq hello-dcl (load_dialog "examples/hello.dcl"))
(new_dialog "hello" hello-dcl)
(start_dialog)
(unload_dialog hello-dcl)

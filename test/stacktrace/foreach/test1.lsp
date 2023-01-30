(setq funs '(+ nil * ))

(defun call_fun (fun a b)
  (apply fun (list a b)))

(defun main ()
  (foreach fun funs
      (call_fun fun 1 2)))

(main)

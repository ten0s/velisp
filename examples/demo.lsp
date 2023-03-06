;;;; SPDX-License-Identifier: 0BSD

(load "util.lsp")

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(if (is_autocad)
    (progn
      (load "../lib/dcl/consts.lsp")
      (load "../lib/velisp/list.lsp")
      (load "../lib/velisp/string.lsp")))

;;;;
;;;; Main Logic
;;;;

(defun editor ( / os)
  (setq os (get_os))
  (cond ((= os "Linux")   '("xdg-open"))
        ((= os "Windows") '("notepad"))
        ((= os "MacOS")   '("open" "-t"))
        (T (alert (strcat "No Editor known for " os)) '("echo"))))

(defun get_current_file_path ()
  (if (is_autocad) (findfile "demo.lsp")
    (getvar "VELISP-FILE")))

(defun get_current_file_dir ()
  ;; Determine current LSP file directory
  ;; Works with either:
  ;; $ velisp examples/demo.lsp
  ;; > (load "examples/demo.lsp")
  ;; > (startapp (argv 0) "examples/demo.lsp")
  (vl-filename-directory (get_current_file_path)))

(defun file_path (dir name ext)
  (strcat dir (path_sep) name ext))

(defun lsp_path (dir name)
  (file_path dir name ".lsp"))

(defun dcl_path (dir name)
  (file_path dir name ".dcl"))

(defun get_names (dir / names name)
  ;; Determine LSP file names that have corresponding DCL files
  (foreach lsp (vl-directory-files dir "*.lsp" LIST_FILES_ONLY)
           (setq name (vl-filename-base lsp))
           (if (vl-file-size (dcl_path dir name))
               (setq names (cons name names))))
  (sort < names))

(setq DIR (get_current_file_dir)
      NAMES (get_names DIR))

(defun init_listbox_names ()
  ;; Initialize listbox with names
  (start_list "listbox_names")
    (mapcar 'add_list NAMES)
  (end_list)

  (action_tile "listbox_names" "(show_current_name)")
  (set_tile "listbox_names" "0"))

(defun get_current_name ()
  ;; Get current selected name
  (nth (atoi (get_tile "listbox_names")) NAMES))

(defun show_current_name ( / name dcl lsp)
  ;; Show DCL and LSP source code for currently selected name
  (setq name (get_current_name))

  (setq dcl (dcl_path DIR name))
  (set_tile "text_dcl" dcl)
  (start_list "listbox_dcl")
    (mapcar 'add_list (read_lines dcl))
  (end_list)

  (setq lsp (lsp_path DIR name))
  (set_tile "text_lsp" lsp)
  (start_list "listbox_lsp")
    (mapcar 'add_list (read_lines lsp))
  (end_list))

(defun run_name ( / name lsp argv0)
  ;; Run currently selected name
  (setq name (get_current_name))
  (setq lsp (lsp_path DIR name))
  (if (is_autocad)
      (alert (strcat "Can't run '" name "' inside AutoCAD"))
    (progn
      (setq argv0 (argv 0))
      (println (strcat "Run " (vl-princ-to-string argv0) " " lsp))
      (startapp argv0 lsp))))

(defun open_file (path_func / name path args)
  (setq name (get_current_name)
        path (path_func DIR name)
        args (append (editor) (list path)))
  (apply 'startapp args))

(defun open_dcl ()
  (open_file dcl_path))

(defun open_lsp ()
  (open_file lsp_path))

;;;;
;;;; DCL Dialog
;;;;

(with_dialog
 "demo.dcl" "demo_dlg" ""
 (lambda ()
   (action_tile "button_run" "(run_name)")
   (action_tile "button_dcl" "(open_dcl)")
   (action_tile "button_lsp" "(open_lsp)")
   (action_tile "button_exit" "(done_dialog 0)")
   (init_listbox_names)
   (show_current_name))
 nil)

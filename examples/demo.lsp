;;;; SPDX-License-Identifier: 0BSD

(setq dcl_file "demo.dcl")
(setq dlg_id "demo_dlg")

(if (< (setq dcl_id (load_dialog dcl_file)) 0)
  (progn
    (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
    (exit 1)))

(if (not (new_dialog dlg_id dcl_id))
  (progn
    (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
    (exit 1)))

(defun println (what)
  (princ what)
  (princ "\n"))

(defun get_file_dir ()
  ;; Determine current LSP file directory
  ;; Works with either:
  ;; $ velisp examples/demo.lsp
  ;; > (load "examples/demo.lsp")
  (vl-filename-directory %VELISP_LSP_FILE%))

(defun file_path (dir name ext)
  (strcat dir "/" name ext))

(defun lsp_path (dir name)
  (file_path dir name ".lsp"))

(defun dcl_path (dir name)
  (file_path dir name ".dcl"))

(defun get_names (dir / names name)
  ;; Determine LSP file names that have corresponding DCL files
  (setq names '())
  (foreach lsp (vl-directory-files dir "*.lsp")
           (setq name (vl-filename-base lsp))
           (if (vl-file-size (dcl_path dir name))
               (setq names (cons name names))))
  (sort '< names))

(setq DIR (get_file_dir))
(setq NAMES (get_names DIR))

(defun init_listbox_names ()
  ;; Initialize listbox with names
  (start_list "listbox_names" START_LIST_CLEAR)
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
  (set_tile "text_dcl" (strcat "DCL File: " dcl))
  (start_list "listbox_dcl" START_LIST_CLEAR)
    (mapcar 'add_list (read_lines dcl))
  (end_list)

  (setq lsp (lsp_path DIR name))
  (set_tile "text_lsp" (strcat "LSP File: " lsp))
  (start_list "listbox_lsp" START_LIST_CLEAR)
    (mapcar 'add_list (read_lines lsp))
  (end_list))

(defun read_lines (file / fd line lines)
  ;; Read lines from file
  (setq fd (open file "r"))
  (while (setq line (read-line fd))
    (setq lines (cons line lines)))
  (close fd)
  (reverse lines))

(defun run_name ( / name lsp argv0)
  ;; Run currently selected name
  (setq name (get_current_name))
  (setq lsp (lsp_path DIR name))
  (setq argv0 (argv 0))
  (println (strcat "Run " (vl-princ-to-string argv0) " " lsp))
  (startapp argv0 lsp))

(init_listbox_names)

(action_tile "run" "(run_name)")
(action_tile "exit" "(done_dialog 0)")

(start_dialog)
(unload_dialog dcl_id)
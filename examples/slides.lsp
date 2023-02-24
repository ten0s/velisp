;;;; SPDX-License-Identifier: 0BSD

(load "util.lsp")

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(if (is_autocad)
    (progn
      (load "../lib/dcl/colors.lsp")
      (load "../lib/dcl/consts.lsp")))

;;;;
;;;; Shell
;;;;

(load "shell.lsp")

;;;;
;;;; Utilities
;;;;

(defun println (what)
  (princ what)
  (princ "\n"))

;;;;
;;;; Main Logic
;;;;

(defun parse_slide_lib_info (lines / parse_line parse_val parsers parsed info)
  (defun parse_line (key str)
    (cons key (parse_val str)))
  (defun parse_val (str)
    (vl-string-trim " " (cadr (split ":" str))))
  (setq parsers
        '(("*Type*:*"   . (lambda (s) (parse_line 'type s)))
          ("*Name*:*"   . (lambda (s) (parse_line 'name s)))
          ("*Size*:*"   . (lambda (s) (parse_line 'size s)))
          ("*Slides*:*" . (lambda (s) (parse_line 'slides s)))))
  (foreach line lines
           (foreach parser parsers
                    (if (wcmatch line (car parser))
                        (progn
                          (setq parsed ((cdr parser) line))
                          (setq info (cons parsed info))))))
  info)

(defun slide_lib_info (slb_file / lines)
  (setq lines (shell (strcat "slide --info=info " slb_file) T))
  (parse_slide_lib_info lines))

(defun slide_lib_names (slb_file / lines)
  (setq lines (shell (strcat "slide --info=names " slb_file) T))
  (mapcar '(lambda (name) (strcase name T))
          lines))

(defun slide_names_from_slb_file (slb_file / lib names)
  (println (slide_lib_info slb_file))

  (setq lib (vl-filename-base slb_file)
        names (slide_lib_names slb_file))
  (mapcar '(lambda (name) (strcat lib "(" name ")"))
          names))

(defun collect_slide_names (dir / slb_files slb_names sld_files sld_names)
  (setq slb_files (vl-directory-files dir "*.slb" 1)
        slb_files (mapcar '(lambda (file) (strcat dir "/" file))
                          slb_files)
        slb_names (mapcar 'slide_names_from_slb_file
                          slb_files)
        slb_names (apply 'append slb_names)
        sld_files (vl-directory-files dir "*.sld" 1)
        sld_names (mapcar 'vl-filename-base
                          sld_files))
  (append slb_names sld_names))

;;;;
;;;; DCL Dialog
;;;;

(setq dcl_file "slides.dcl")
(setq dlg_id "slides_dlg")

(if (< (setq dcl_id (load_dialog dcl_file)) 0)
  (progn
    (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
    (exit 1)))

(if (not (new_dialog dlg_id dcl_id))
  (progn
    (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
    (exit 1)))

(defun fill_slide_names ()
  (start_list "names" START_LIST_CLEAR)
    (mapcar 'add_list NAMES)
  (end_list)

  (action_tile "names" "(draw_current_slide)")
  (set_tile "names" "0"))

(defun get_current_name ()
  (nth (atoi (get_tile "names")) NAMES))

(defun draw_current_slide ( / name)
  (setq name (get_current_name))

  (start_image "image")
    (fill_image 0 0 WIDTH HEIGHT BLACK_COLOR)
    (slide_image 0 0 WIDTH HEIGHT name)
  (end_image))

(setq NAMES (collect_slide_names "examples"))
(setq WIDTH  (dimx_tile "image"))
(setq HEIGHT (dimy_tile "image"))

(fill_slide_names)
(draw_current_slide)

(start_dialog)
(unload_dialog dcl_id)

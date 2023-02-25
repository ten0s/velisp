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
  (princ what) (princ "\n")
  what)

(defun inspect (msg what)
  (princ msg) (princ " ")
  (println what))

(defun flatten (xss)
  ;; Poor man's flatten
  (apply 'append xss))

(defun shell_lines (cmd)
  (shell (inspect "Running:" cmd) SHELL_RETURN_LINES))

(defun get_dir_files (dir glob / join_dir)
  (defun join_dir (file)
    (strcat dir "/" file))
  (mapcar 'join_dir (vl-directory-files dir glob LIST_FILES_ONLY)))

;;;;
;;;; Main Logic
;;;;

(defun parse_lines (lines parsers / parsed parsers* pattern parser result)
  (foreach line lines
           (setq parsed nil
                 parsers* parsers)
           (while (and (not parsed) parsers*)
             (setq pattern (caar parsers*)
                   parser  (cdar parsers*))
             (if (wcmatch line pattern)
                 (setq parsed (parser line)
                       result (cons parsed result))
               (setq parsers* (cdr parsers*)))))
  (reverse result))

(defun parse_colon_split_info (lines / parse_line)
  (defun parse_line (str / pair)
    (setq pair (split ":" str))
    (strcat (vl-string-trim " " (car pair))
            "\t:  "
            (vl-string-trim " " (cadr pair))))
  (parse_lines lines
               '(("*:*" . parse_line))))

(defun get_slb_info (slb_file / lines)
  (setq lines (shell_lines (strcat "slide --info=info " slb_file)))
  (parse_colon_split_info lines))

(defun get_slb_names (slb_file / lines)
  (setq lines (shell_lines (strcat "slide --info=names " slb_file)))
  (mapcar '(lambda (name) (strcase name LOWER_CASE)) lines))

(defun get_infos_from_slb_file (slb_file / slb_name slb_info names)
  (setq slb_name (vl-filename-base slb_file)
        slb_info (get_slb_info slb_file)
        names    (get_slb_names slb_file))
  (mapcar '(lambda (name)
             (list (cons 'name (strcat slb_name "(" name ")"))
                   (cons 'short_name name)
                   (cons 'file slb_file)
                   (cons 'lib_info slb_info)))
          names))

(defun get_slide_info_from_sld_file (sld_file / lines)
  (setq lines (shell_lines (strcat "slide --info=info " sld_file)))
  (parse_colon_split_info lines))

(defun get_slide_info_from_slb_file (slb_file name / lines)
  (setq lines (shell_lines (strcat "slide --info=info " slb_file " " name)))
  (parse_colon_split_info lines))

(defun get_slide_records_from_sld_file (sld_file)
  (shell_lines (strcat "slide --info=recs " sld_file)))

(defun get_slide_records_from_slb_file (slb_file name)
  (shell_lines (strcat "slide --info=recs " slb_file " " name)))

(defun get_slide_info (info)
  (if (get_info 'lib_info info)
      (get_slide_info_from_slb_file (get_info 'file info)
                                    (get_info 'short_name info))
    (get_slide_info_from_sld_file (get_info 'file info))))

(defun get_slide_records (info)
  (if (get_info 'lib_info info)
      (get_slide_records_from_slb_file (get_info 'file info)
                                       (get_info 'short_name info))
    (get_slide_records_from_sld_file (get_info 'file info))))

(defun get_info_from_sld_file (sld_file / name)
  (setq name (vl-filename-base sld_file))
  (list (cons 'name name)
        (cons 'file sld_file)))

(defun get_infos_from_dir_files (dir / slb_files slb_infos sld_files sld_names)
  (setq slb_files (get_dir_files dir "*.slb")
        slb_infos (flatten (mapcar 'get_infos_from_slb_file slb_files))
        sld_files (get_dir_files dir "*.sld")
        sld_infos (mapcar 'get_info_from_sld_file sld_files))
  (append slb_infos sld_infos))

;; (key [(key . val)]) -> val
(defun get_info (key info)
  (cdr (assoc key info)))

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

(defun fill_slide_names ( / names)
  (setq names (mapcar '(lambda (info) (get_info 'name info)) INFOS))
  (start_list "names" START_LIST_CLEAR)
    (mapcar 'add_list names)
  (end_list)
  (set_tile "names" "0")
  (action_tile "names" "(process_current_name)"))

(defun get_current_info ()
  (nth (atoi (get_tile "names"))
       INFOS))

(defun process_current_name ( / info)
  (setq info (get_current_info))
  (draw_slide (get_info 'name info))
  (show_info (get_slide_info info)
             (get_info 'lib_info info)
             (get_slide_records info)))

(defun draw_slide (name)
  (start_image "image")
    (fill_image  0 0 WIDTH HEIGHT BLACK_COLOR)
    (slide_image 0 0 WIDTH HEIGHT name)
  (end_image))

(defun show_info (slide_info lib_info slide_records)
  (start_list "slide-info" START_LIST_CLEAR)
    (mapcar 'add_list slide_info)
  (end_list)

  (start_list "lib-info" START_LIST_CLEAR)
    (mapcar 'add_list lib_info)
  (end_list)

  (start_list "slide-recs" START_LIST_CLEAR)
    (mapcar 'add_list slide_records)
  (end_list))

(setq INFOS (get_infos_from_dir_files "examples"))
(setq WIDTH  (dimx_tile "image"))
(setq HEIGHT (dimy_tile "image"))

(fill_slide_names)
(process_current_name)

(start_dialog)
(unload_dialog dcl_id)

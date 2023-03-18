;;;; SPDX-License-Identifier: 0BSD

(load "util.lsp")

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(if (is_autocad)
    (progn
      (load "../lib/dcl/colors.lsp")
      (load "../lib/dcl/consts.lsp")
      (load "../lib/kernel/consts.lsp")
      (load "../lib/velisp/string.lsp")))

;;;;
;;;; Shell
;;;;

(load "shell.lsp")

;;;;
;;;; Utilities
;;;;

(defun flatten (xss)
  ;; Poor man's flatten
  (apply 'append xss))

(defun shell_lines (cmd)
  (shell (inspect "Running: " cmd) SHELL_RETURN_LINES))

(defun get_dir_files (dir glob / join_dir)
  (defun join_dir (file)
    (strcat dir (path_sep) file))
  (mapcar 'join_dir (vl-directory-files dir glob LIST_FILES_ONLY)))

;;;;
;;;; Main Logic
;;;;

(defun get_current_file_path ()
  (if (is_autocad) (findfile "slides.lsp")
    (getvar "VELISP-FILE")))

(defun get_current_file_dir ()
  ;; Determine current LSP file directory
  (vl-filename-directory (get_current_file_path)))

(defun parse_lines (lines parsers / parsed parsers* pattern parser result)
  (foreach line lines
           (setq parsed nil
                 parsers* parsers)
           (while (and (not parsed) parsers*)
             (setq pattern (caar parsers*)
                   parser  (cdar parsers*))
             (if (wcmatch line pattern)
                 (setq parsed (apply parser (list line))
                       result (cons parsed result))
               (setq parsers* (cdr parsers*)))))
  (reverse result))

(defun parse_colon_split_lines (lines / parse_line)
  (defun parse_line (str / pair is_win)
    (setq pair (split ":" str)
          is_win (and (= (get_os) "Windows")
                      (not (is_autocad))))
    (strcat (vl-string-trim " " (car pair))
            (if is_win " " "") ; An extra space needed in Windows
            "\t:  "
            (vl-string-trim " " (cadr pair))))
  (parse_lines lines
               '(("*:*" . parse_line))))

(defun get_slb_info (slb_file / lines)
  (setq lines (shell_lines (strcat "slide-info --what=info \"" slb_file "\"")))
  (parse_colon_split_lines lines))

(defun get_slb_names (slb_file / lines)
  (setq lines (shell_lines (strcat "slide-info --what=names \"" slb_file "\"")))
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
  (setq lines (shell_lines (strcat "slide-info --what=info \"" sld_file "\"")))
  (parse_colon_split_lines lines))

(defun get_slide_info_from_slb_file (slb_file name / lines)
  (setq lines (shell_lines (strcat "slide-info --what=info \"" slb_file "\" " name)))
  (parse_colon_split_lines lines))

(defun get_slide_records_from_sld_file (sld_file)
  (shell_lines (strcat "slide-info --what=records \"" sld_file "\"")))

(defun get_slide_records_from_slb_file (slb_file name)
  (shell_lines (strcat "slide-info --what=records \"" slb_file "\" " name)))

(defun get_version_info ()
  (car (shell_lines "slide-info --version")))

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

(defun fill_slide_names (names)
  (start_list "names")
    (mapcar 'add_list names)
  (end_list)
  (set_tile "names" "0")
  (action_tile "names" "(process_current_name)"))

(defun get_current_info ( / name infos* info found)
  (setq name (nth (atoi (get_tile "names")) NAMES)
        infos* INFOS)
  (while (and infos* (not found))
    (setq info (car infos*)
          infos* (cdr infos*)
          found (= (get_info 'name info) name)))
  info)

(defun process_current_name ( / info slide_info slide_recs)
  (setq info (get_current_info))

  ;; Get slide info if not cached
  (if (not (get_info 'slide_info info))
      (setq slide_info (get_slide_info info)
            info (cons (cons 'slide_info slide_info) info)))

  ;; Get side records if not cached
  (if (not (get_info 'slide_recs info))
      (setq slide_recs (get_slide_records info)
            info (cons (cons 'slide_recs slide_recs) info)))

  ;; Cache if needed
  (if (or slide_info slide_recs)
      (setq INFOS (cons info INFOS)))

  (draw_slide (get_info 'name info))
  (show_info (get_info 'slide_info info)
             (get_info 'slide_recs info)
             (get_info 'lib_info info)))

(defun draw_slide (name)
  (start_image "image")
    (fill_image  0 0 WIDTH HEIGHT COLOR_BLACK)
    (slide_image 0 0 WIDTH HEIGHT name)
  (end_image))

(defun show_info (slide_info slide_recs lib_info)
  (start_list "slide-info")
    (mapcar 'add_list slide_info)
  (end_list)

  (start_list "slide-recs")
    (mapcar 'add_list slide_recs)
  (end_list)

  (start_list "lib-info")
    (mapcar 'add_list lib_info)
  (end_list))

(defun show_version ()
  (alert (strcat "Uses slide-info v" VERSION " under the hood\n"
                 "See https://github.com/ten0s/slide for detail")))

(with_dialog
 "slides.dcl" "slides_dlg" ""
 (lambda ()
   (setq DIR (get_current_file_dir)
         INFOS (get_infos_from_dir_files DIR)
         NAMES (mapcar '(lambda (info) (get_info 'name info)) INFOS)
         VERSION (get_version_info)
         WIDTH (dimx_tile "image")
         HEIGHT (dimy_tile "image"))

   (action_tile "info" "(show_version)")

   (fill_slide_names NAMES)
   (process_current_name))
 nil)

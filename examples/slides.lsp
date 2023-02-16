;;;; SPDX-License-Identifier: 0BSD

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(defun is_autocad ()
  (not (getvar "VELISP-VERSION")))

(if (is_autocad)
    (progn
      (load "../lib/dcl/colors.lsp")
      (load "../lib/dcl/consts.lsp")))

;;;;
;;;; Globals
;;;;

;;;;
;;;; Utilities
;;;;

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

(setq NAMES '("mylib(slide1)" "mylib(slide2)" "slide1" "slide2" "slide_dn" "slide_kss" "slide_up"))
(setq WIDTH  (dimx_tile "image"))
(setq HEIGHT (dimy_tile "image"))

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
    (fill_image 0 0 width height BLACK_COLOR)
    (slide_image 0 0 width height name)
  (end_image))

(fill_slide_names)
(draw_current_slide)

(start_dialog)
(unload_dialog dcl_id)

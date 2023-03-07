;;;; SPDX-License-Identifier: 0BSD

;; mode_tile
(setq MODE_TILE_ENABLE 0
      MODE_TILE_DISABLE 1
      MODE_TILE_FOCUS 2
      MODE_TILE_SELECT_EDITBOX 3
      MODE_TILE_FLIP_IMAGE_HIGHLIGHT 4)

;; start_list operation
(setq START_LIST_CHANGE 1
      START_LIST_APPEND 2
      START_LIST_CLEAR 3)

;; action $reason(s) for
;; edit_box, list_box, image_button, and slider tiles
(setq ACTION_REASON_SELECTED 1
      ACTION_REASON_FOCUS_LOST 2
      ACTION_REASON_INTERIM_CHANGE 3
      ACTION_REASON_DOUBLE_CLICK 4)

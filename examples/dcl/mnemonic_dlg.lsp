; Draw M on imagebutton1
(setq w1 (dimx "imagebutton1"))
(setq h1 (dimy "imagebutton1"))
(setq dw1 (fix (/ w1 4)))
(setq dh1 (fix (/ h1 4)))

(start_image "imagebutton1")
(vector_image dw1 (* dh1 3) dw1 dh1 0)
(vector_image dw1 dh1 (* dw1 2) (* dh1 2) 0)
(vector_image (* dw1 2) (* dh1 2) (* dw1 3) dh1 0)
(vector_image (* dw1 3) dh1 (* dw1 3) (* dh1 3)  0)
(end_image)

; Draw N on imagebutton2
(setq w2 (dimx "imagebutton2"))
(setq h2 (dimy "imagebutton2"))
(setq dw2 (fix (/ w2 4)))
(setq dh2 (fix (/ h2 4)))

(start_image "imagebutton2")
(vector_image dw2 (* dh2 3) dw2 dh2 0)
(vector_image dw2 dh2 (* dw2 3) (* dh2 3) 0)
(vector_image (* dw2 3) (* dh2 3) (* dw2 3) dh2 0)
(end_image)

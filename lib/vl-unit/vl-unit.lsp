;;;
;;; VL-Unit - Visual Lisp Unit testing framework is a unit test tool for Visual Lisp.
;;; Copyright (c) 2010, Dmitry Klionsky <dm.klionsky@gmail.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the author nor the names of its contributors may
;;;       be used to endorse or promote products derived from this software without
;;;       specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;;
;;; Globals
;;;

(setq *vlu-tests* '())

(setq *vlu-test-count* 0)
(setq *vlu-test-pass-count* 0)
(setq *vlu-test-fail-count* 0)

(setq *vlu-assert-count* 0)
(setq *vlu-assert-pass-count* 0)
(setq *vlu-assert-fail-count* 0)

;;;
;;; Public functions
;;;

(defun vlu-add-test (test-fn)
  (cond ((not (member test-fn *vlu-tests*))
     (setq *vlu-tests* (cons test-fn *vlu-tests*)) t)))

(defun vlu-remove-test (test-fn)
  (cond ((member test-fn *vlu-tests*)
     (setq *vlu-tests* (vl-remove test-fn *vlu-tests*)) t)))

(defun vlu-remove-tests ()
  (cond ((null *vlu-tests*) nil)
    (t (setq *vlu-tests* '()) t)))

(defun vlu-get-tests ()
  *vlu-tests*)

(defun vlu-run-test (test-fn / result)
  (%vlu-inc-test-count)
  (%vlu-reset-assert-count)
  (%vlu-reset-assert-pass-count)
  (%vlu-reset-assert-fail-count)
  (apply test-fn '())
  (setq result (%vlu-get-test-result))
  (if result
      (%vlu-inc-test-pass-count)
      (%vlu-inc-test-fail-count))
  (%vlu-show-test-result-message test-fn)
  result)

(defun vlu-run-tests ()
  (%vlu-reset-test-count)
  (%vlu-reset-test-pass-count)
  (%vlu-reset-test-fail-count)
  (mapcar 'vlu-run-test (vlu-get-tests))
  (%vlu-show-tests-result-message)
  (%vlu-get-tests-result))

(defun vlu-assert (actual)
  (%vlu-inc-assert-count)
  (cond ((and actual)
     (%vlu-inc-assert-pass-count) t)
    (t
     (%vlu-show-assert-fail-message t actual)
     (%vlu-inc-assert-fail-count) nil)))

(defun vlu-assert-not (actual)
  (%vlu-inc-assert-count)
  (cond ((and (not actual))
     (%vlu-inc-assert-pass-count) t)
    (t
     (%vlu-show-assert-fail-message nil actual)
     (%vlu-inc-assert-fail-count) nil)))

(defun vlu-assert-equal (expected actual)
  (%vlu-inc-assert-count)
  (cond ((equal expected actual)
     (%vlu-inc-assert-pass-count) t)
    (t
     (%vlu-show-assert-fail-message expected actual)
     (%vlu-inc-assert-fail-count) nil)))

;;;
;;; Private functions
;;;

(defun %vlu-show-assert-fail-message (expected actual)
  (princ
   (strcat
    "Expected: " (vl-princ-to-string expected)
    " but saw: " (vl-princ-to-string actual)
    "\n")))

(defun %vlu-show-test-result-message (test-fn)
  (princ
   (strcat
    (vl-symbol-name test-fn) ": "
    (itoa *vlu-assert-pass-count*) " assertions passed, "
    (itoa *vlu-assert-fail-count*) " failed.\n")))

(defun %vlu-show-tests-result-message ()
  (princ
   (strcat
    "TOTAL: "
    (itoa *vlu-test-pass-count*) " tests passed, "
    (itoa *vlu-test-fail-count*) " failed, "
    (itoa (- *vlu-test-count* *vlu-test-pass-count* *vlu-test-fail-count*))
    " execution errors.\n")))

(defun %vlu-get-test-result ()
  (= (- *vlu-assert-count* *vlu-assert-pass-count*) 0))

(defun %vlu-get-tests-result ()
  (= (- *vlu-test-count* *vlu-test-pass-count*) 0))

(defun %vlu-reset-test-count ()
  (setq *vlu-test-count* 0))

(defun %vlu-reset-test-pass-count ()
  (setq *vlu-test-pass-count* 0))

(defun %vlu-reset-test-fail-count ()
  (setq *vlu-test-fail-count* 0))

(defun %vlu-reset-assert-count ()
  (setq *vlu-assert-count* 0))

(defun %vlu-reset-assert-pass-count ()
  (setq *vlu-assert-pass-count* 0))

(defun %vlu-reset-assert-fail-count ()
  (setq *vlu-assert-fail-count* 0))

(defun %vlu-inc-test-count ()
  (setq *vlu-test-count* (1+ *vlu-test-count*)))

(defun %vlu-inc-test-pass-count ()
  (setq *vlu-test-pass-count* (1+ *vlu-test-pass-count*)))

(defun %vlu-inc-test-fail-count ()
  (setq *vlu-test-fail-count* (1+ *vlu-test-fail-count*)))

(defun %vlu-inc-assert-count ()
  (setq *vlu-assert-count* (1+ *vlu-assert-count*)))

(defun %vlu-inc-assert-pass-count ()
  (setq *vlu-assert-pass-count* (1+ *vlu-assert-pass-count*)))

(defun %vlu-inc-assert-fail-count ()
  (setq *vlu-assert-fail-count* (1+ *vlu-assert-fail-count*)))

'ok

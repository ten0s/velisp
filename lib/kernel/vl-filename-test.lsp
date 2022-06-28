;;;; SPDX-License-Identifier: 0BSD

(vlu-add-test
 (defun vl-filename-base-test ()
   (vlu-assert-equal "acad" (vl-filename-base "c:\\acadwin\\acad.exe"))
   (vlu-assert-equal "acadwin" (vl-filename-base "c:\\acadwin"))

   (vlu-assert-equal "acad" (vl-filename-base "c:/acadwin/acad.exe"))
   (vlu-assert-equal "acadwin" (vl-filename-base "c:/acadwin"))

   (vlu-assert-equal "utilities" (vl-filename-base "/myutilities/lsp/utilities.lsp"))
   (vlu-assert-equal "support" (vl-filename-base "/myutilities/support"))
   ))

(vlu-add-test
 (defun vl-filename-directory-test ()
   (vlu-assert-equal "c:\\acadwin" (vl-filename-directory "c:\\acadwin\\template.txt"))

   (vlu-assert-equal "c:/acadwin" (vl-filename-directory "c:/acadwin/template.txt"))

   (vlu-assert-equal "/myutilities/support" (vl-filename-directory "/myutilities/support/template.txt"))

   (vlu-assert-equal "" (vl-filename-directory "template.txt"))
   ))

(vlu-add-test
 (defun vl-filename-extension-test ()
   (vlu-assert-equal ".txt" (vl-filename-extension "c:\\acadwin\\output.txt"))
   (vlu-assert-equal nil (vl-filename-extension "c:\\acadwin\\output"))

   (vlu-assert-equal ".txt" (vl-filename-extension "c:/acadwin/output.txt"))
   (vlu-assert-equal nil (vl-filename-extension "c:/acadwin/output"))

   (vlu-assert-equal ".txt" (vl-filename-extension "/myutilities/support/output.txt"))
   (vlu-assert-equal nil (vl-filename-extension "/myutilities/support/output"))
   ))

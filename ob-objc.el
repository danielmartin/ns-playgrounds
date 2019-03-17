;;; ob-objc.el --- Org-Babel support for the Objective C programming language.

;; Copyright (C) 2019 Daniel Martín

;; Author: Daniel Martín <mardani29@yahoo.es> inspired by code by Álvaro Ramírez
;; URL: http://github.com/danielmartin/ns-playgrounds
;; Keywords: languages
;; Version: 0.0.1
;; Created: March 9th, 2019

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Org-Babel support for the Objective C programming language.
;;

;;; Code:
(require 'ob)

(defcustom org-babel-objc-compile-command "clang -x objective-c -framework Foundation"
  "For example: \"clang -x objective-c -framework Foundation\"."
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-execute:objc (body params)
  "Compile Objective-C BODY with org PARAMS and execute binary."
  (let* ((src-file (org-babel-temp-file "org-babel-objc-block-" ".m"))
         (cmpflag (or (cdr (assq :cmpflag params)) ""))
         (full-body (org-babel-expand-body:generic body params))
         (bin-file
          (org-babel-process-file-name
           (org-babel-temp-file "org-babel-objc-block" org-babel-exeext))))
    (with-temp-file src-file (insert full-body))
    (org-babel-eval
     (concat org-babel-objc-compile-command " " cmpflag " " src-file " " "-o" " " bin-file) "")

    ;; Using 2>&1 since org babel does not include stderr in output from NSLog.
    (let ((results (org-babel-eval (concat (org-babel-process-file-name bin-file) " 2>&1")  "")))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assq :result-params params))
         (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "c-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
       (org-babel-pick-name
        (cdr (assq :rowname-names params)) (cdr (assq :rownames params)))))))

(provide 'ob-objc)
;;; ob-objc.el ends here

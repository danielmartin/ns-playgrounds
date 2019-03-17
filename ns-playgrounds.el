;;; ns-playgrounds.el --- Code playgrounds for Apple languages

;; Copyright (C) 2019 Daniel Martín

;; Author: Daniel Martín <mardani29@yahoo.es>
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
;; Code playgrounds for Apple languages.
;;
;; This package adds Swift and Objective C support to org-babel.  For
;; Swift, support includes choosing among installed toolchain
;; versions, or an experimental feature where you can debug the Swift
;; compiler using a code snippet as input.
;;

;;; Code:
(with-eval-after-load "org"
  (add-to-list 'org-babel-load-languages '(swift . t))
  (add-to-list 'org-babel-load-languages '(objc . t))
  (org-babel-do-load-languages
   'org-babel-load-languages org-babel-load-languages))

(provide 'ns-playgrounds)
;;; ns-playgrounds.el ends here

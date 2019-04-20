;;; ob-swift.el --- Org-Babel support for the Swift programming language.

;; Copyright (C) 2019 Daniel Martín

;; Author: Daniel Martín <mardani29@yahoo.es>
;; URL: http://github.com/danielmartin/ns-playgrounds
;; Keywords: languages
;; Version: 0.0.1
;; Created: March 9th, 2019
;; Package-Requires: ((emacs "24.4") (dash "2.14.1") (f "0.20.0"))

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
;; Org-Babel support for the Swift programming language.
;;

;;; Code:
(require 'ob) ;; Org-Babel
(require 'dash) ;; For -map, -flatten, -if-let
(require 'f) ;; For searching available Swift toolchains.

(defvar ob-swift-debug-compiler-path nil
  "Path to a debug+assert Swift compiler.
You must configure this variable if you want to use the 'x-ray-this'
feature.")

(defvar ob-swift-prompt-if-no-toolchain nil
  "Whether you want to interactively be asked for a toolchain when `:toolchain' is not set.
By default, this property is nil, which means that you won't be
interactively asked for a Swift toolchain when a code snippet
does not have a `:toolchain' tag. In that case, the default Swift
toolchain that is configured on this computer will be used.")

(defvar ob-swift-toolchain-dirs '("/Library/Developer/Toolchains/")
  "Directories that can contain Swift toolchains (.xctoolchain).")

(defun ob-swift-sdk-path-flag ()
  "Prepares a SDK flag to use with the Swift compiler."
  (format "-sdk %s" (replace-regexp-in-string
                                      "\n\\'" ""
                                      (shell-command-to-string "echo $(xcrun --show-sdk-path --sdk macosx)"))))

(defun ob-swift-debug-args (temp-file)
  "Returns a list of debug arguments to prepare the Swift
  compiler instrumentation."
  (append `("-frontend", "-c", temp-file, "-Xllvm", "-swift-diagnostics-assert-on-error=1") `,(-flatten (split-string (ob-swift-sdk-path-flag)))))

(defun ob-swift--toolchain-id-in-dir (dir)
  "Returns a pair (toolchain name . toolchain ID) represented by
DIR."
  `(,(file-name-base dir) ,(nth 0 (process-lines "/usr/libexec/PlistBuddy"
                                                 "-c" "Print CFBundleIdentifier:" (format "%s/Info.plist" dir)))))

(defun ob-swift--toolchain-directories ()
  "Returns a list of Swift toolchain directories installed on
this computer."
  (-map (lambda (dir)
          (f-directories (expand-file-name dir)))
        ob-swift-toolchain-dirs))

(defun ob-swift--collect-toolchains ()
  "Collects all Swift toolchains installed on this computer."
  (-map (lambda (dir)
          ;; Skip the swift-latest.xctoolchain symbolic link.
          (unless (and
                   (file-symlink-p dir)
                   (string-equal (file-name-nondirectory dir) "swift-latest.xctoolchain"))
            (ob-swift--toolchain-id-in-dir dir)))
        (-flatten (ob-swift--toolchain-directories))))

(defun ob-swift--process-stop (debug-session)
  "Process a debugger breakpoint."
  (-if-let (thread-id (dap--debug-session-thread-id debug-session))
      (-if-let (stack-frames (gethash thread-id
                                      (dap--debug-session-thread-stack-frames debug-session)))
          (progn
            (remove-hook 'dap-stack-frame-changed-hook 'ob-swift--process-stop)
            (dap--go-to-stack-frame debug-session (nth 11 stack-frames))))))

(defun ob-swift--x-ray-this (program-content debug-compiler-path dap-server-path)
  "X-Ray PROGRAM-CONTENT using DEBUG-COMPILER.
PROGRAM-CONTENT is a string that represents the Swift code that
should be 'x-rayed'. DEBUG-COMPILER-PATH is a string path to the
debug-assert Swift compiler that will be instrumented.
DAP-SERVER-PATH is a string path to the LLDB debug adapter
binary."
  ;; Create a temp file with the program content.
  (let* ((temp-file (org-babel-temp-file "org-babel-swift-block-" ".swift"))
        (debug-args (ob-swift-debug-args temp-file)))
    (with-temp-file temp-file (insert program-content))
    ;; We're interested to know when a stack frame changes after the
    ;; first stop to process the stack trace further.
    (add-hook 'dap-stack-frame-changed-hook 'ob-swift--process-stop)
    (dap-debug `(:type "lldb"
                       :cwd nil
                       :request "launch"
                       :program ,debug-compiler-path
                       :args ,debug-args
                       :name "LLDB::Run"
                       :dap-server-path ,dap-server-path))))

(defun ob-swift--toolchain-eval (body &optional toolchain)
  "Evaluates BODY using optionally a Swift TOOLCHAIN"
  (let ((temp-file (org-babel-temp-file "org-babel-swift-block-" ".swift")))
    (with-temp-file temp-file (insert body))
    ;; On macOS we can use xcrun which will configure a
    ;; SDKROOT automatically for us.
    (if (eq system-type 'darwin)
        (progn
          (let* ((available-toolchains (ob-swift--collect-toolchains))
                 (selected-toolchain (if (and ob-swift-prompt-if-no-toolchain (not toolchain))
                                         (cadr
                                          (assoc
                                           (completing-read "Select the Swift toolchain that will execute this code snippet: " available-toolchains) available-toolchains))
                                       (or (cadr (assoc toolchain available-toolchains)) "swift"))))
            (message (format "Using %s Swift toolchain..." selected-toolchain))
            (org-babel-eval
             (format "xcrun --toolchain %s swift %s" selected-toolchain temp-file)
             "")))
      (progn
        (org-babel-eval
         (format "swift %s" temp-file)
         "")))))

(defun ob-swift--eval (body &optional toolchain x-ray-this? debug-compiler-path)
  "Evaluates the Swift code in BODY.

If TOOLCHAIN is set, uses that toolchain to execute the code.  If
X-RAY-THIS? is set, instead of executing the code and print the
result, it attaches a debugger to a debug compiler that will stop
at the first encountered error."
  (if x-ray-this?
      (let ((compiler-path (or debug-compiler-path ob-swift-debug-compiler-path))
            (dap-server-path dap-lldb-debug-program))
        (unless compiler-path
          (user-error "Could not find a debug+assert Swift compiler binary. Either pass `:debug-compiler-path' <Path> in your code snippet or set the `ob-swift-debug-compiler-path' variable"))
        (unless dap-server-path
          (user-error "Could not find a debug adapter for LLDB. Please configure the `dap-lldb-debug-program' variable to point to the LLDB debug adapter binary"))
        (ob-swift--x-ray-this body (expand-file-name compiler-path) dap-server-path))
    (ob-swift--toolchain-eval body toolchain)))

;; Main entry point
(defun org-babel-execute:swift (body params)
  "Executes Swift code in BODY, given some PARAMS.

This is the list of supported PARAMS:

:toolchain - Optional. Specifies the Swift toolchain that will execute the
code snippet, for example: 'swift-4.2-DEVELOPMENT-SNAPSHOT-2018-10-30-a'.
Only applicable to macOS systems.

:x-ray-this - Optional. Instead of executing the code, Org-babel
will attach LLDB to a debug Swift binary and break at the first
diagnostic error. Useful for compiler writers or programming
language enthusiasts that want to understand Swift better."
  (let ((toolchain (cdr (assoc :toolchain params)))
        (x-ray-this? (assoc :x-ray-this params)))
    (when (and (not (eq system-type 'darwin)) toolchain)
      (user-error "The :toolchain flag is only supported on macOS systems."))
    (when (and x-ray-this? (or (not (featurep 'dap-mode))
                               (not (featurep 'dap-lldb))))
      (user-error "You need to load `dap-mode' and `dap-lldb' for the x-ray-this feature to work."))
    (ob-swift--eval body toolchain x-ray-this?)))

(provide 'ob-swift)
;;; ob-swift.el ends here

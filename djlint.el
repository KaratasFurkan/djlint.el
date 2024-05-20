;;; djlint.el --- djLint: Lint & Format HTML Templates -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Furkan Karataş

;; Author: Furkan Karataş <furkan.karatas02@gmail.com>
;; URL: https://github.com/KaratasFurkan/djlint.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "28.1"))
;; Keywords: linters, formatters, html

;; This file is not part of GNU Emacs.

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

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'reformatter)

;;;; Customization

(defgroup djlint nil
  "djLint: Lint & Format HTML Templates."
  :group 'convenience
  :prefix "djlint-")

(defcustom djlint-program "djlint"
  "A literal string, or the name of a variable which holds `djlint' program
 path."
  :type 'string
  :group 'djlint)

(defcustom djlint-formatter-args '("--reformat" "-")
  "Command-line arguments for `djlint-program', for formatting purposes. Note
 that `-' is necessary to receive current buffer with stdin."
  :type '(repeat string)
  :group 'djlint)

(defcustom djlint-linter-args '("--lint" "-")
  "Command-line arguments for `djlint-program' for linting purposes. Note that
 `-' is necessary to receive current buffer with stdin."
  :type '(repeat string)
  :group 'djlint)

;;;; Variables

(defvar djlint--flymake-proc nil)
(defvar djlint--flymake-buffer " *djlint-flymake*")
(defvar djlint--flymake-command-parser-regexp "\\([A-Z][0-9]+\\) \\([0-9]+\\):\\([0-9]+\\) \\(.*\\)")

;;;; Commands

;;;###autoload (autoload 'djlint-format-buffer "djlint" nil t)
;;;###autoload (autoload 'djlint-format-region "djlint" nil t)
;;;###autoload (autoload 'djlint-format-on-save-mode "djlint" nil t)
(reformatter-define djlint-format
  :group 'djlint
  :program djlint-program
  :args djlint-formatter-args)

;;;###autoload
(define-minor-mode djlint-mode
  "A minor mode to utilize both formatter and linter features of `djlint'. This
 minor mode activates `flymake-mode' with `djlint-flymake' backend to display
 linter errors, `eldoc-mode' to echo flymake diagnostics in the echo area and
 `djlint-format-on-save-mode' to reformat active buffer on each save."
  :require 'djlint  ; TODO: is this necessary?
  :group 'djlint
  (if djlint-mode
      (progn
        (djlint-format-on-save-mode 1)
        (djlint-setup-flymake-backend)
        (when (not flymake-mode)
          (flymake-mode 1))
        (when (not eldoc-mode)
          (eldoc-mode 1)))
    (djlint-format-on-save-mode -1)
    (remove-hook 'flymake-diagnostic-functions 'djlint-flymake t)))

;;;; Functions

;;;;; Private

(defun djlint--flymake-parse-output (source proc report-fn)
  "Parse output of `djlint-program' + `djlint-linter-args' and collect
 diagnostics for `flymake'."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    ;; Parse the output buffer for diagnostic's messages and
    ;; locations, collect them in a list of objects, and call
    ;; `report-fn'.
    (cl-loop
     ;; TODO: add support for different "linter_output_format"s
     while (search-forward-regexp
            djlint--flymake-command-parser-regexp
            nil t)
     for msg = (format "%s: %s" (match-string 1) (match-string 4))
     for (beg . end) = (flymake-diag-region
                        source
                        (string-to-number (match-string 2))
                        (1+ (string-to-number (match-string 3))))
     for type = :warning
     when (and beg end)
     collect (flymake-make-diagnostic source beg end type msg)
     into diags
     finally (funcall report-fn diags))))

;;;;; Public

(defun djlint-flymake (report-fn &rest _args)
  "A `flymake' backend for `djlint'. The code is mostly taken from
https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html"
  (unless (executable-find djlint-program)
    (error "djlint executable is not found"))
  ;; If a live process launched in an earlier check was found, that process is
  ;; killed. When that process's sentinel eventually runs, it will notice its
  ;; obsoletion, since it have reset `djlint-flymake-proc' to a different value
  (when (process-live-p djlint--flymake-proc)
    (kill-process djlint--flymake-proc))
  ;; Save the current buffer, the narrowing restriction, remove any narrowing
  ;; restriction.
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `djlint--flymake-proc' process to a new process calling
      ;; djlint.
      (setq
       djlint--flymake-proc
       (make-process
        :name "djlint-flymake"
        :noquery t
        :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        :buffer (generate-new-buffer djlint--flymake-buffer)
        :command `(,djlint-program ,@djlint-linter-args)
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might be simply
          ;; suspended.
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as `djlint--flymake-proc',
                ;; which indicates that `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc djlint--flymake-proc))
                    (djlint--flymake-parse-output source proc report-fn)
                  (flymake-log :warning "Canceling obsolete check %s" proc))
              ;; Cleanup the temporary buffer used to hold the check's output.
              (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdin, followed by an EOF.
      (process-send-region djlint--flymake-proc (point-min) (point-max))
      (process-send-eof djlint--flymake-proc))))

;;;###autoload
(defun djlint-setup-flymake-backend ()
  "Add `djlint' flymake backend to `flymake-diagnostic-functions'."
  (add-hook 'flymake-diagnostic-functions 'djlint-flymake nil t))

;;;; Footer

(provide 'djlint)

;;; djlint.el ends here

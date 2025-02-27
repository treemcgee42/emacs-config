;;; tm42-dumbgrep.el --- Lookup symbols by grepping  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <vmalladi@vmalladi-vniCounters.sjc.aristanetworks.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defun tm42/dumbgrep/generate-grep-string (re &optional file-extension)
  (let ((prefix "rg -nS --no-heading")
        (suffix
         (pcase file-extension
           ('py "-tpy")
           (_ (format "-g '*.%s'" file-extension))))
        (root (if-let ((project (project-current nil)))
                  (project-root project)
                "")))
    (string-join (list prefix
                       (format "'%s'" re)
                       suffix
                       root)
                 " ")))

(defun tm42/dumbgrep/python-definition (name)
  (when (string-match-p "python" (symbol-name major-mode))
    (let* ((re (format "(def|class) %s" name)))
      (tm42/dumbgrep/generate-grep-string re 'py))))

(defvar tm42/dumbgrep/generators
  (list #'tm42/dumbgrep/python-definition)
  "List of functions used to generate grep command strings. Specifically,
a \"generator\" is a function that
- takes a single NAME parameter, which is the thing to look up
- returns NIL if the function should not handle this request, essentially deferring
  the search to another generator
- returns a grep command string if it can handle the request.")

(defun tm42/dumbgrep (&optional name)
  (interactive)
  (let* ((name (if name name (thing-at-point 'symbol)))
         (grep-cmd
          (catch 'break
            (dolist (generator tm42/dumbgrep/generators)
              (when-let ((grep-cmd (funcall generator name)))
                (message "grep-cmd: %s" grep-cmd)
                (setq grep-cmd
                      (read-string "Run grep (like this): "
                                   grep-cmd))
                (throw 'break grep-cmd))))))
    (if grep-cmd
        (grep grep-cmd)
      (message "Couldn't guess the grep!"))))

(provide 'tm42-dumbgrep)
;;; tm42-dumbgrep.el ends here

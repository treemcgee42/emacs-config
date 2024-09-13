;;; tm42-header.el --- Helps create interactive header lines  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author: Varun Malladi <varun.malladi@gmail.com>
;; Keywords: maint

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

;; This project was inspired by, and borrows from, the header line in
;; the rg package.

;;; Code:

(require 'eieio)
(require 'mouse)

(defvar-local tm42-header/bd nil
  "Data relevant to header lines managed by the tm42-header package.")

;; --- Utility functions --------------------------------------------

(defun tm42-header/trim-or-pad-string (str len)
  "If STR has length less than LEN, pad STR with spaces until it
has length LEN. If STR has length greater than LEN, trim STR
until it has length LEN."
  (let ((str-len (string-width str)))
    (cond ((> str-len len)
           (truncate-string-to-width str len))
          ((< str-len len)
           (string-pad str len))
          (t str))))

(defun tm42-header/trim-or-pad-header-content (f-spec left-padding right-padding)
  "Given a header line format specification F-SPEC, trim it or pad
it with trailing spaces such that there is exactly the sum of
LEFT-PADDING and RIGHT-PADDING space left. The intended use is to
specify the header line format by first listing an item of size
LEFT-PADDING, then the result of this function, and finally an
item of size RIGHT-PADDING."
  (let* ((header-len (window-width))
         (main-content-len (- header-len left-padding right-padding)))
    (tm42-header/trim-or-pad-string (format-mode-line f-spec)
                                    main-content-len)))

;; --- Header line components and helpers ---------------------------

(defun tm42-header/render-label (labelform)
  "Return a fontified header label.
LABELFORM is either a string to render or a form where the `car' is a
conditional and the two following items are then and else specs.
Specs are lists where the the `car' is the labels string and the
`cadr' is font to use for that string."
  (list '(:propertize "[" font-lock-face (header-line bold))
        (cond
         ((stringp labelform)
          `(:propertize ,labelform font-lock-face (header-line bold)))
         ((listp labelform)
          (let* ((condition (nth 0 labelform))
                 (then (nth 1 labelform))
                 (else (nth 2 labelform)))
            `(:eval (if ,condition
                        (propertize ,(nth 0 then) 'font-lock-face '(,(nth 1 then) header-line bold))
                      (propertize ,(nth 0 else) 'font-lock-face '(,(nth 1 else) header-line bold))))))
         (t (error "Not a string or list")))
        '(:propertize "]" font-lock-face (header-line bold))
        '(": ")))

(defun tm42-header/mouse-action (command help &rest items)
  "Add a keymap with mouse click action for COMMAND.
When hovering HELP is shown as a tooltip.  ITEMS is the header line
items that the map will be applied to."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
      (lambda (click)
        (interactive "e")
        (message "clicked")
        (mouse-select-window click)
        (call-interactively command)))
    `(:propertize ,items mouse-face header-line-highlight
                  help-echo ,help
                  keymap ,map)))

(defun tm42-header/next-page-button ()
  "Generate a button that can be interacted with to go to the next
header page."
  (tm42-header/mouse-action
   (lambda ()
     (interactive)
     (tm42-header/next-header-page tm42-header/bd))
   "Next header page" " > "))

(defun tm42-header/previous-page-button ()
    "Generate a button that can be interacted with to go to the previous
header page."
  (tm42-header/mouse-action
   (lambda ()
     (interactive)
     (tm42-header/next-header-page tm42-header/bd -1))
   "Next header page" " < "))

;; --- Buffer data class --------------------------------------------

(defclass tm42-header/buffer-data ()
  ((pages
    :initarg :pages
    :documentation "List of format specifications representing the
header line pages.")
   (current-page
    :documentation "Index in `pages' representing which page is
currently displayed.")
   (current-format
    :documentation "The format specification used to generate the
currently visible header line."))
  "Data (buffer-local) associated with the tm42-header feature.")

(cl-defmethod tm42-header/draw-header ((bd tm42-header/buffer-data))
  "Draw the header described by BD in the current window."
  (setq header-line-format (slot-value bd 'current-format)))

(cl-defmethod tm42-header/next-header-page ((bd tm42-header/buffer-data) &optional n)
  "Change the currently displayed page in the header by cycling N
times. Positive N goes forward in the list and negative N goes
backwards, both wrapping around so the target page is always
valid."
  (let* ((n (if n n 1))
         (new-page (mod (+ (slot-value bd 'current-page) n)
                        (length (slot-value bd 'pages))))
         (new-format (nth new-page (slot-value bd 'pages))))
    (message "new-page: %d num-pages: %d" new-page (length (slot-value bd 'pages)))
    (setf (slot-value bd 'current-page) new-page)
    (setf (slot-value bd 'current-format) new-format))
  (tm42-header/draw-header bd))

;; --- Header line creation API -------------------------------------

(defun tm42-header/header-line (&optional hide show)
  "When called with no arguments, this function toggles the header
line of the current buffer.. If HIDE is not nil then the header
line will be hidden regardless of whether it is visible or
not. Similarly for if SHOW is not nil. HIDE takes precedence over
SHOW.

This function assumes tm42-header has already initialized itself
in the current buffer."
  (interactive)
  (when (not tm42-header/bd)
    (error "tm42-header not initialized for this buffer"))
  (cond (hide
         (setq header-line-format nil))
        (show
         (tm42-header/draw-header tm42-header/bd))
        (header-line-format
         (tm42-header/header-line t nil))
        (t
         (tm42-header/header-line nil t))))

(defun tm42-header/make-paged-header (pages &optional hide-initially)
  "A paged header is a list of headers than can be navigated
between. For example, the first page is what is displayed in the
header first, and a button will be generated that, when clicked,
will then display the second page in the header.

PAGES is a list of header line format specifications (which are
the same as mode line format specifications). Next/previous
buttons, padding, and trimming will be generated by this
function. For most use cases the caller does not need to concern
themselves with the length of their page, aside from choosing it
to be sized such that important content is not cut off at
targetted window sizes.

Sets the current buffer's buffer-local `tm42-header/bd' variable
with the associated data.  If HIDE-INITIALLY is nil, the header
line for the current buffer is displayed."
  (let* ((current-page -1)
         (num-pages (length pages))
         (prev-page-button (tm42-header/previous-page-button))
         (prev-page-button-len (string-width (format-mode-line prev-page-button)))
         (next-page-button (tm42-header/next-page-button))
         (next-page-button-len (string-width (format-mode-line next-page-button)))
         (formatted-pages
          (mapcar
           (lambda (page)
             (setq current-page (+ current-page 1))
             (let* ((is-first (= current-page 0))
                    (is-last (= current-page (- num-pages 1)))
                    (left-padding (if is-first 0 prev-page-button-len))
                    (right-padding (if is-last 0 next-page-button-len)))
               (append `(,(if (not is-first)
                              prev-page-button
                            ""))
                       `(,(tm42-header/trim-or-pad-header-content
                           page
                           left-padding right-padding))
                       `(,(if (not is-last)
                              next-page-button
                            "")))))
           pages))
         (bd (tm42-header/buffer-data :pages formatted-pages)))
    (setf (slot-value bd 'current-format) (car formatted-pages))
    (setf (slot-value bd 'current-page) 0)
    (setq tm42-header/bd bd))
  (tm42-header/header-line hide-initially t))

(provide 'tm42-header)
;;; tm42-header.el ends here

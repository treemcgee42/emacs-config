;;; tm42-maximize-window.el --- Window maximization scheme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author: Varun Malladi <varun.malladi@gmail.com>
;; Keywords: convenience

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

;; This package builds on core windowing ideas and code.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'window)

;; --- Helpers ------------------------------------------------------

(defun tm42/--window-shrinkable-amount (window &optional horizontal)
  "Returns the maximum amount by which WINDOW can shrink (from its
current size) while still having the minimum acceptable window
size. If HORIZONTAL is nil, this works on the window height,
otherwise it works on the window width."
  (max (- (window-size window horizontal nil 'floor) ; child total window size
          (window-min-size window horizontal)) ; child minimum size
       0))

(defun tm42/window-max-delta (window horizontal noup nodown
                                     &optional delta delta-components)
  "Wrapper around `window-max-delta' that treats the NODOWN
parameter completely differently.

WINDOW, HORIZONTAL, and NOUP have the same meaning as in
`window-max-delta'. In fact, when NODOWN is nil, they are passed
directly to that function.  When NODOWN is not nil, however, this
function does not factor internal sibling windows into its
computation.

A list is returned, whose CAR is the max-delta and whose CDR is a
list representing the components of that computation. The CDR is
a list of `window-delta's."
  (if (not (window-parent window))
      (list 0 nil)
    (let ((child (window-child (window-parent window)))
          (delta (if delta delta 0))
          (delta-components (if delta-components delta-components '())))
      (when (window-combined-p child horizontal)
        (while child
          (cond ((eql child window)
                 nil)
                ((and nodown (not window-live-p child))
                 nil)
                (t
                 (let* ((delta-increment (tm42/--window-shrinkable-amount
                                          child horizontal)))
                   (push (make-window-delta :window child
                                            :delta delta-increment
                                            :horizontal horizontal)
                         delta-components)
                   (setq delta (+ delta delta-increment)))))
          (setq child (window-right child))))
      (if noup
          (list delta delta-components)
        (tm42/window-max-delta window horizontal noup nodown
                               delta delta-components)))))

;; --- Structs ------------------------------------------------------

(cl-defstruct window-delta
  window
  delta
  horizontal)

;; --- tm42/maximizing-window ---------------------------------------

(defclass tm42/maximizing-window ()
  ((window
    :initarg :window
    :type window
    :documentation "The window being maximized.")
   (maximizing-horizontally
    :initform nil
    :documentation "Nil if maximizing vertically, non-nil if maximizing
horizontally.")
   (minimized-components
    :initform nil
    :documentation "A list of `window-delta' representing the sizes of
minimized windows before this window got maximized.")
   (maximized-size
    :initform nil
    :documentation "The size of WINDOW according to HORIZONTAL after
maximization.")))

(cl-defmethod tm42/--maximizing-window-maximize
  ((mw tm42/maximizing-window)
   mode)
  "Maximize WINDOW according to MODE.

We introduce the following terminology:

- (window) axis: the direction in which a window is combined
  within its parent. Practically, this can be thought of as the
  direction in which a window is split.
- parent, sibling, child: these mean the same as in the usual Emacs
  window sense.

MODE may have the following values:
- axiswise-siblings: maximize WINDOW along its axis, but only allow
  its siblings to be shrunk.
- axiswise-siblings-and-children: maximize WINDOW along its axis, but
  only allow its siblings and children to be shrunk."
  (let* ((window (slot-value mw 'window))
         (stacked-vertically (window-combined-p window))
         (stacked-horizontally (window-combined-p window t))
         (noup (or (eql mode 'axiswise-siblings)
                   (eql mode 'axiswise-siblings-and-children)))
         (nodown (or (eql mode 'axiswise-siblings)))
         (delta nil))
    (cond
     (stacked-vertically
      (setq delta (tm42/window-max-delta window nil noup nodown))
      (window-resize window (car delta))
      (setf (slot-value mw 'maximizing-horizontally) nil
            (slot-value mw 'minimized-components) (nth 1 delta)
            (slot-value mw 'maximized-size) (car delta)))
     (stacked-horizontally
      (setq delta (tm42/window-max-delta window t noup nodown))
      (window-resize window (car delta) t)
      (setf (slot-value mw 'maximizing-horizontally) t
            (slot-value mw 'minimized-components) (nth 1 delta)
            (slot-value mw 'maximized-size) (car delta)))
     (t
      nil))))

(cl-defmethod tm42/--maximizing-window-is-maximized
  ((mw tm42/maximizing-window))
  (if (slot-value mw 'maximized-size)
      t
    nil))

(cl-defmethod tm42/--maximizing-window-changed-size
  ((mw tm42/maximizing-window))
  "Determine if MW's window's current size is different than it
was expanded to."
  (let* ((window (slot-value mw 'window))
         (horizontal (slot-value mw 'maximizing-horizontally))
         (current-size (window-size window horizontal))
         (cached-size (slot-value mw 'maximized-size)))
    (= current-size cached-size)))

(cl-defmethod tm42/--maximizing-window-unmaximize
  ((mw tm42/maximizing-window))
  "If a minimized window has been deleted, ignore it."
  (let ((success t))
    (dolist (mc (slot-value mw 'minimized-components))
      (when (window-valid-p (window-delta-window mc))
        (setq success
              (ignore-errors
                (window-resize
                 (window-delta-window mc)
                 (window-delta-delta mc)
                 (window-delta-horizontal mc))))))
    (when (not success)
      (message "tm42-maximize-window: Could not fully restore previous window configuration."))))
        

;; --- State --------------------------------------------------------

(defvar tm42/maximizing-windows '())

(defun tm42/create-maximizing-window (window)
  (add-to-list 'tm42/maximizing-windows
               `(,window . ,(make-instance 'tm42/maximizing-window
                                           :window window))))

(defun tm42/delete-maximizing-window (window)
  (setq tm42/maximizing-windows
        (assq-delete-all window tm42/maximizing-windows)))

;; --- Public API ---------------------------------------------------

(defun tm42/maximize-clicked-window (click)
  "Wrapper around `tm42/maximize-window' that determines the window
to expand based on CLICK. Clicking again restores the layout."
  (interactive "e")
  (when (and (eq (posn-area (event-end click))
                   'mode-line)
               (eq (posn-window (event-end click))
                   (posn-window (event-start click))))
    (let* ((window (posn-window (event-start click)))
           (mw (alist-get window tm42/maximizing-windows)))
      (when (not mw)
        (tm42/create-maximizing-window window)
        (setq mw (alist-get window tm42/maximizing-windows)))
      (if (tm42/--maximizing-window-is-maximized mw)
          (progn 
            (tm42/--maximizing-window-unmaximize mw)
            (tm42/delete-maximizing-window window))
        (tm42/--maximizing-window-maximize
         mw 'axiswise-siblings-and-children)))))

(provide 'tm42-maximize-window)
;;; tm42-maximize-window.el ends here

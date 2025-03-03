;;; tm42-scrolling-elt.el --- Helps create interactive header lines  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

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

;; Provides text elements with fixed width that scroll over time to reveal
;; the full content.

;;; Code:

(require 'eieio)

(defclass scrolling-modeline-elt ()
  ((get-content-fn
    :initarg :get-content-fn
    :documentation "Function to call to get the full content for the element.")
   (char-width
    :initarg :char-width
    :documentation "Number of characters wide the element is.")
   (current-content
    :initarg :current-content
    :documentation "The full content that is currently being used.")
   (current-content-start-idx
    :initarg :current-content-start-idx
    :documentation "The first character the element is showing."))
  "Scrolling TODO")

(defun make-scrolling-modeline-elt (get-content-fn width)
  (scrolling-modeline-elt
   :get-content-fn get-content-fn
   :char-width width
   :current-content (concat (funcall get-content-fn) "~~")
   :current-content-start-idx 0))

(cl-defmethod render-scrolling-modeline-elt ((sme scrolling-modeline-elt))
  (cl-assert (<= (slot-value sme 'current-content-start-idx)
                 (length (slot-value sme 'current-content))))
  (let* ((remaining-string
          (let* ((start-idx (slot-value sme 'current-content-start-idx))
                 (end-idx (min (length (slot-value sme 'current-content))
                               (+ start-idx (slot-value sme 'char-width)))))
            (substring (slot-value sme 'current-content)
                       start-idx end-idx)))
         (wrapped-string
          (let ((end-idx (max (- (slot-value sme 'char-width)
                                 (length remaining-string))
                              0)))
            (substring (slot-value sme 'current-content)
                       0 end-idx))))
    (concat remaining-string wrapped-string)))

(cl-defmethod tick-scrolling-modeline-elt ((sme scrolling-modeline-elt))
  (let ((new-content (concat (funcall (slot-value sme 'get-content-fn)) "~~")))
    (when (not (string= new-content (slot-value sme 'current-content)))
      (setf (slot-value sme 'current-content) new-content)
      (setf (slot-value sme 'current-content-start-idx) 0)))
  (let ((rendered-output (render-scrolling-modeline-elt sme)))
    (setf (slot-value sme 'current-content-start-idx)
          (mod (+ (slot-value sme 'current-content-start-idx) 1)
               (length (slot-value sme 'current-content))))
    rendered-output))

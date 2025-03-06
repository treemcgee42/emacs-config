;;; tm42-scrolling-elt.el --- Helps create interactive header lines  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Varun Malladi <varun.malladi@gmail.com>

;;; Commentary:

;; Provides text elements with fixed width that scroll over time to reveal
;; the full content.

;;; TODOs:

;; - Documentation for class.

;; --- begin public api -------------------------------------------------------------

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
    :documentation "The first character the element is showing.")
   (last-tick-time
    :initform (float-time)
    :documentation "Last time we scrolled. Used to determine if we should scroll."))
  "Scrolling TODO")

(defcustom tm42/scrolling-elt-separator "  "
  "When wrapping the scrolling element content, display this in between the start
and the end when both are visible.")

(defcustom tm42/scrolling-elt-tick-frequency 1
  "The (minimum) number of seconds between ticks, e.g. scrolling one character.")

(defun tm42/make-scrolling-modeline-elt (get-content-fn width)
  (scrolling-modeline-elt
   :get-content-fn get-content-fn
   :char-width width
   :current-content (concat (funcall get-content-fn) "  ")
   :current-content-start-idx 0))

(cl-defmethod tm42/tick-scrolling-modeline-elt ((sme scrolling-modeline-elt))
  (when (--tm42/scrolling-elt/should-tick (slot-value sme 'last-tick-time))
    (setf (slot-value sme 'last-tick-time) (float-time))
    ;; Check if content has updated
    (let ((new-content (concat (funcall (slot-value sme 'get-content-fn)) "  ")))
      (when (not (string= new-content (slot-value sme 'current-content)))
        (setf (slot-value sme 'current-content) new-content)
        (setf (slot-value sme 'current-content-start-idx) 0)))
    (when (> (length (slot-value sme 'current-content))
             (slot-value sme 'char-width))
      (setf (slot-value sme 'current-content-start-idx)
            (mod (+ (slot-value sme 'current-content-start-idx) 1)
                 (length (slot-value sme 'current-content))))))
  (--tm42/scrolling-elt/render-sme sme))

;; --- end public api ---------------------------------------------------------------
;; --- begin helpers ----------------------------------------------------------------

(defun --tm42/scrolling-elt/should-tick (last-tick-time)
  "LAST-TICK-TIME should be the float-time from the last tick (e.g. the number of
seconds since the epoch, as a float)."
  (> (- (float-time) last-tick-time) tm42/scrolling-elt-tick-frequency))

(defun --tm42/scrolling-elt/wrap-string (st width start-idx)
  (let* ((remaining-string
          (let ((end-idx (min (length st) (+ start-idx width))))
            (substring st start-idx end-idx)))
         (wrapped-string
          (let ((end-idx (max (- width (length remaining-string))
                              0)))
            (substring st 0 end-idx))))
    (concat remaining-string wrapped-string)))

(cl-defmethod --tm42/scrolling-elt/render-sme ((sme scrolling-modeline-elt))
  (cl-assert (<= (slot-value sme 'current-content-start-idx)
                 (length (slot-value sme 'current-content))))
  (if (<= (length (slot-value sme 'current-content))
          (slot-value sme 'char-width))
      (slot-value sme 'current-content)
    (--tm42/scrolling-elt/wrap-string
     (concat (slot-value sme 'current-content) tm42/scrolling-elt-separator)
     (slot-value sme 'char-width)
     (slot-value sme 'current-content-start-idx))))

;; --- end helpers ------------------------------------------------------------------

(provide 'tm42-scrolling-elt)

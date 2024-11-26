;;; tm42-compilation-status.el --- Status for compilation and compilation-adjacent jobs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <vmalladi@vmalladi-vniFeature.sjc.aristanetworks.com>

(require 'eieio)

;; --- Job state -------------------------------------------------------------------

(defclass tm42/compilation-info ()
  ((status :initarg :status)
   (buffer :initarg :buffer)
   (description :initarg :description)
   (time-start :initform (current-time))
   (time-end :initform nil))
  "Information about a single compilation job.")

(cl-defmethod tm42/--compilation-info-finish ((obj tm42/compilation-info) passed)
  "TODO"
  (setf (slot-value obj 'time-end) (current-time))
  (setf (slot-value obj 'status) (if passed 'passed 'failed)))

(cl-defmethod tm42/compilation-info-elapsed-time ((obj tm42/compilation-info))
  "Get the time since the job has started. If the job has finished, get the
difference between the end time and start time."
  (time-subtract (slot-value obj 'time-end) (slot-value obj 'time-start)))

(cl-defmethod tm42/compilation-info-print-tabular ((obj tm42/compilation-info))
  "Print the object information in a nice aligned row."
  (let ((elapsed-string
         (format-seconds "%.2hh:%.2mm:%.2ss"
                         (float-time (tm42/compilation-info-elapsed-time obj))))
        (desc-string (slot-value obj 'description)))
    (format "%s  |  %s" elapsed-string desc-string)))

;; --- End job state ---------------------------------------------------------------
;; --- Hooks -----------------------------------------------------------------------

(defun tm42/--compile-advice-around (compile-fn &rest args)
  "Creates a `tm42/compilation-info' instance for the given compilation invocation.
Intended to be a around advice for `compile'."
  (let* ((command (car args))
         ;; This hook is called in the compilation before before executing the
         ;; command. We do most of the work here so we know what buffer we are
         ;; in.
         (compilation-start-hook
          (lambda (p)
            (let ((info (tm42/compilation-info :status 'running
                                               :buffer (current-buffer)
                                               :description command)))
              (setq-local tm42/compilation-info-var info)
              (tm42/compilation-info-state-add-job info)))))
    (apply compile-fn args)))

(advice-add 'compile :around #'tm42/--compile-advice-around)

(defun tm42/--compilation-status-compile-finish-hook (buffer msg)
  "Update the buffer-local info variable when the compilation has finished.
Safe to add to `compilation-finish-functions' because it's a no-op if the
variable doesn't exist."
  (when tm42/compilation-info-var
    (let ((passed (string-match "^finished" msg)))
      (tm42/--compilation-info-finish tm42/compilation-info-var passed))))

(add-hook 'compilation-finish-functions
          #'tm42/--compilation-status-compile-finish-hook)

;; --- End hooks -------------------------------------------------------------------
;; --- State -----------------------------------------------------------------------

(defclass tm42/compilation-info-state-class ()
  ((jobs :initform '()))
  "State for the tm42/compilation-info package.")
(defvar tm42/compilation-info-state (tm42/compilation-info-state-class))

(cl-defmethod tm42/compilation-info-state-class-add-job
  ((obj tm42/compilation-info-state-class) info)
  "TODO"
  (push info (slot-value obj 'jobs)))
(defun tm42/compilation-info-state-add-job (info)
  (tm42/compilation-info-state-class-add-job tm42/compilation-info-state info))

(cl-defmethod tm42/compilation-info-state-class-passes
  ((obj tm42/compilation-info-state-class))
  "Returns a list of passed compilation jobs."
  (seq-filter (lambda (el)
                (eql (slot-value el 'status) 'passed))
              (slot-value obj 'jobs)))
(defun tm42/compilation-info-state-passes ()
  (tm42/compilation-info-state-class-passes tm42/compilation-info-state))

(cl-defmethod tm42/compilation-info-state-class-failures
  ((obj tm42/compilation-info-state-class))
  "Returns a list of passed compilation jobs."
  (seq-filter (lambda (el)
                (eql (slot-value el 'status) 'failed))
              (slot-value obj 'jobs)))
(defun tm42/compilation-info-state-failures ()
  (tm42/compilation-info-state-class-failures tm42/compilation-info-state))

(cl-defmethod tm42/compilation-info-state-class-runnings
  ((obj tm42/compilation-info-state-class))
  "Returns a list of passed compilation jobs."
  (seq-filter (lambda (el)
                (eql (slot-value el 'status) 'running))
              (slot-value obj 'jobs)))
(defun tm42/compilation-info-state-runnings ()
  (tm42/compilation-info-state-class-runnings tm42/compilation-info-state))

(cl-defmethod tm42/compilation-info-state-class-clear
  ((obj tm42/compilation-info-state-class))
  "Clear all stored jobs."
  (setf (slot-value obj 'jobs) '()))
(defun tm42/compilation-info-clear ()
  "Clear all stored jobs."
  (interactive)
  (tm42/compilation-info-state-class-clear tm42/compilation-info-state))

;; --- End state -------------------------------------------------------------------

(defun tm42/compilation-status ()
  (interactive)
  (let ((buffer (get-buffer-create "*compilation-status*"))
        (passes (tm42/compilation-info-state-passes))
        (failures (tm42/compilation-info-state-failures))
        (runnings (tm42/compilation-info-state-runnings)))
    (if (get-buffer-window buffer)
        (select-window (get-buffer-window buffer))
      (switch-to-buffer buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    ;; In progress
    (insert "\n")
    (insert (propertize (format "--- %d running ---\n" (length runnings))
                        'face '(:foreground "orange")))
    (dolist (running runnings)
      (insert (format "%s\n" (tm42/compilation-info-print-tabular running))))
    ;; Failures
    (insert "\n")
    (insert (propertize (format "--- %d failures ---\n" (length failures))
                        'face '(:foreground "red")))
    (dolist (fail failures)
      (insert (format "%s\n" (tm42/compilation-info-print-tabular fail))))
    ;; Passes
    (insert "\n")
    (insert (propertize (format "--- %d passes ---\n" (length passes))
                        'face '(:foreground "green")))
    (dolist (pass passes)
      (insert (format "%s\n" (tm42/compilation-info-print-tabular pass))))
    (tm42-compilation-status-mode)
    (setq buffer-read-only t)))

(defvar-keymap tm42-compilation-status-mode-map
  :doc "Keymap for the tm42's compilation-status."
  "g" #'tm42/compilation-status
  "c" (lambda ()
        (interactive)
        (when (y-or-n-p "Clear compilation status?")
          (tm42/compilation-info-clear)
          (tm42/compilation-status))))

(define-derived-mode tm42-compilation-status-mode special-mode "TM42CS"
  "Major mode for viewing tm42's compilation status."
  :keymap tm42-compilation-status-map)

(defun tm42/tab-bar-compilation-status ()
  (propertize
   (concat "Comp: "
           (propertize (format "%d" (length tm42/compilation-status-passes))
                       'face '(:foreground "green")
                       'help-echo (format "%d passes" (length tm42/compilation-status-passes)))
           " "
           (propertize (format "%d" (length tm42/compilation-status-failures))
                       'face '(:foreground "red")
                       'help-echo (format "%d failures" (length tm42/compilation-status-failures))))
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] #'tm42/compilation-status)
             map)))

(provide 'tm42-compilation-status)

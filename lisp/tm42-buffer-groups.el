;;; tm42-buffer-groups.el --- Organize buffers into logical groups  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Varun Malladi

;; Author: Varun Malladi <varun.malladi@gmail.com>
;; URL: https://example.com/package-name.el (TODO)
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: buffer group order cycle

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

;;; Commentary:

;; This package allows flanges to be easily frobnicated.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'package-name)

;;;; Usage

;; Run one of these commands:

;; `package-name-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `package-name' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'eieio)

;;;; Customization

(defgroup tm42-buffer-groups nil
  "Settings for `tm42-buffer-groups'."
  :link '(url-link "https://example.com/package-name.el"))

(defcustom tm42/bg/debug-tracing-enabled nil
  "Enable debug messages for the tm42-buffer-groups."
  :type 'boolean
  :group 'tm42-buffer-groups)

;;;; Variables

(defclass tm42-buffer-groups-state-class ()
  ((group-to-buffers-map :initarg :group-to-buffers-map
                          :initform (make-hash-table :test 'equal))
   (buffer-to-groups-map :initarg :buffer-to-groups-map
                          :initform (make-hash-table :test 'equal))))

(defvar tm42/bg/state (tm42-buffer-groups-state-class))

(cl-defmethod init-with-groups ((class (subclass tm42-buffer-groups-state-class))
                                group-lists)
  "Initialize a tm42-buffer-groups-state-class with group-lists.
Each element of group-lists should be a a list whose first element is the name of the
group, and whose remaining elements are the buffer names in that group."
  (let ((group-to-buffers-map (make-hash-table :test 'equal))
        (buffer-to-groups-map (make-hash-table :test 'equal)))
    (dolist (group-list group-lists)
      (let ((group-name (car group-list))
            (group-bufs (cdr group-list)))
        (cl-assert (stringp group-name))
        (cl-assert (listp group-lists))
        (cl-assert (not (gethash group-name group-to-buffers-map)))
        (puthash group-name group-bufs group-to-buffers-map)
        (dolist (group-buf group-bufs)
          (cl-assert (stringp group-buf))
          (cl-assert (not (member group-name (gethash group-buf buffer-to-groups-map '()))))
          (puthash group-buf
                   (append (gethash group-buf buffer-to-groups-map '())
                           (list group-name))
                   buffer-to-groups-map))))
    (tm42-buffer-groups-state-class :group-to-buffers-map group-to-buffers-map
                                    :buffer-to-groups-map buffer-to-groups-map)))

(cl-defmethod get-bufs-in-group ((state tm42-buffer-groups-state-class)
                                 group-name
                                 &optional dflt)
  "Returns the buffers belonging to group-name."
  (gethash group-name (slot-value state 'group-to-buffers-map) dflt))
(cl-defmethod get-groups-of-buf ((state tm42-buffer-groups-state-class)
                                 buf-name
                                 &optional dflt)
  "Returns the groups to which buf-name belongs to."
  (gethash buf-name (slot-value state 'buffer-to-groups-map) dflt))
(cl-defmethod get-groups ((state tm42-buffer-groups-state-class))
  "Returns a list of all groups."
  (let ((groups-list '()))
    (maphash #'(lambda (group-name buffers)
                 (setq groups-list (append groups-list (list group-name))))
             (slot-value state 'group-to-buffers-map))
    groups-list))

(cl-defmethod assign-buf-to-group ((state tm42-buffer-groups-state-class) buf-name group-name)
  (let ((groups-of-buf (get-groups-of-buf state buf-name '()))
        (bufs-in-group (get-bufs-in-group state group-name '())))
    (when (not (member group-name groups-of-buf))
      (tm42/bg/debug-message "assigning buffer '%s' to group '%s'" buf-name group-name)
      (setq groups-of-buf (append groups-of-buf (list group-name))))
    (puthash buf-name groups-of-buf (slot-value state 'buffer-to-groups-map))
    (when (not (member buf-name bufs-in-group))
      (setq bufs-in-group (append bufs-in-group (list buf-name))))
    (puthash group-name bufs-in-group (slot-value state 'group-to-buffers-map))))
    

(ert-deftest tm42/bg/state-init-test ()
  (let* ((test-cases '((("group1" "a" "b" "c"))
                       (("group1" "a") ("group2" "b" "c"))
                       (("group1" "a") ("group2" "a" "b")))))
     (dolist (test-case test-cases)
       (let ((state (init-with-groups 'tm42-buffer-groups-state-class test-case)))
         (dolist (group-list test-case)
           (let ((group-name (car group-list))
                 (group-bufs (cdr group-list)))
             (should (equal (get-bufs-in-group state group-name)
                            group-bufs))
             (dolist (group-buf group-bufs)
               (should (member group-name (get-groups-of-buf state group-buf))))))))))

(ert-deftest tm42/bg/assign-buf-to-group-test ()
  (let ((state (tm42-buffer-groups-state-class)))
    ;; Assign nonexistent Buf1 to nonexistent Grp1.
    (assign-buf-to-group state "Buf1" "Grp1")
    (should (member "Buf1" (get-bufs-in-group state "Grp1")))
    (should (member "Grp1" (get-groups-of-buf state "Buf1")))
    ;; Assign nonexistent Buf2 to existent Grp1.
    (assign-buf-to-group state "Buf2" "Grp1")
    (should (member "Buf2" (get-bufs-in-group state "Grp1")))
    (should (member "Grp1" (get-groups-of-buf state "Buf2")))
    ;; Assign existent Buf2 to nonexistent Grp2.
    (assign-buf-to-group state "Buf2" "Grp2")
    (should (member "Buf2" (get-bufs-in-group state "Grp2")))
    (should (member "Grp2" (get-groups-of-buf state "Buf2")))
    ;; Check nothing was overwritten.
    (should (member "Buf1" (get-bufs-in-group state "Grp1")))
    (should (member "Buf2" (get-bufs-in-group state "Grp1")))
    (should (member "Buf2" (get-bufs-in-group state "Grp2")))
    (should (member "Grp1" (get-groups-of-buf state "Buf1")))
    (should (member "Grp1" (get-groups-of-buf state "Buf2")))
    (should (member "Grp2" (get-groups-of-buf state "Buf2")))))

;;;; Commands

;; TODO: when to use autoload?

(defun tm42/bg/clear-groups ()
  (interactive)
  (setq tm42/bg/state (tm42-buffer-groups-state-class)))

(defun tm42/bg/organize-buffers ()
  "Organize buffers into user-defined groups."
  (interactive)
  (let ((buf (get-buffer-create "*tm42-bg-organize-buffers*")))
    (tm42/bg/refresh-buffer-groups)
    (with-current-buffer buf
      (erase-buffer)
      (insert "---")
      (insert "\n")
      (dolist (group-name (get-groups tm42/bg/state))
        (tm42/bg/debug-message "[tm42/bg/organize-buffers] printing group '%s'"
                               group-name)
        (let ((bufs-in-group (get-bufs-in-group tm42/bg/state group-name)))
          (insert "\nGROUP: " group-name "\n")
          (dolist (buf-in-group bufs-in-group)
            (insert buf-in-group "\n"))))
      (insert "\n\n")
      (insert "---")
      (fundamental-mode)
      (switch-to-buffer buf))))

(defun tm42/bg/organize-buffers-update ()
  "Parse the *tm42-bg-organize-buffers* buffer to update state."
  (interactive)
  (let ((groups '())
        (current-group nil)
        (current-group-bufs '())
        (started nil))
    (with-current-buffer "*tm42-bg-organize-buffers*"
      (goto-char (point-min))
      (while (and (not started) (not (eobp)))
        (setq started (looking-at "---"))
        (forward-line 1))
      (while (and (not (eobp)) (not (looking-at "---")))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))          
          (if (string-match-p "^[[:space:]]*$" line)
              (when current-group
                (setq groups (append groups
                                     (list (append (list current-group)
                                                   current-group-bufs))))
                (setq current-group nil)
                (setq current-group-bufs '()))
            (if (string-prefix-p "GROUP: " line)
                (setq current-group (substring line 7 nil))
              (setq current-group-bufs (append current-group-bufs (list line)))))
          (forward-line 1))))
    (setq tm42/bg/state (init-with-groups 'tm42-buffer-groups-state-class groups))))

(defun tm42/bg/next-buffer ()
  (interactive)
  (tm42/bg/forward-buffer 1))
(defun tm42/bg/previous-buffer ()
  (interactive)
  (tm42/bg/forward-buffer -1))

;;;; Functions

;;;;; Public

(defun tm42/bg/refresh-buffer-groups ()
  "Assigns any buffers which are already not grouped to a group."
  (let ((buf-list (mapcar 'buffer-name (buffer-list))))
    (dolist (buf buf-list)
      (when (and (not (get-groups-of-buf tm42/bg/state buf))
                 (tm42/bg/should-group-buffer buf))
        (tm42/bg/debug-message
         "[tm42/bg/refresh-buffer-groups] found unassigned buffer '%s'"
         buf)
        (assign-buf-to-group tm42/bg/state buf "Other")))))

(defun tm42/bg/forward-buffer (n)
  "Switch forward n buffers in the current group. Negative n means backwards."
  (let* ((group-name (car (get-groups-of-buf tm42/bg/state (buffer-name))))
         (buffers-in-group (get-bufs-in-group tm42/bg/state group-name))
         (current-buf-idx (cl-position (buffer-name) buffers-in-group :test 'string=)))
    (switch-to-buffer (tm42/bg/cyclic-nth n current-buf-idx buffers-in-group))))

;;;;; Private

;; TODO: fix this hook
;; (defun tm42-new-buffer-hook (frame)
;;   (when (not (active-minibuffer-window))
;;     (let* ((win (frame-selected-window frame))
;;            (new-buf-name (buffer-name (window-buffer win)))
;;            (prev-buf-name (buffer-name (car (car (window-prev-buffers win)))))
;;            (prev-buf-group (tm42-buffer-groups-get-group-for-buffer prev-buf-name)))
;;       (when (not (gethash new-buf-name tm42-buffer-group-buffer-to-group-map))
;;         (tm42-buffer-groups-debug-message
;;          "adding buffer %s to group %s of prev buffer %s"
;;          new-buf-name prev-buf-group prev-buf-name)
;;         (tm42-assign-buffer-to-group prev-buf-group new-buf-name)))))
;; (defun tm42-kill-buffer-hook ()
;;   (tm42-remove-buffer-from-buffer-groups (buffer-name)))
;; (add-hook 'window-buffer-change-functions #'tm42-new-buffer-hook)
;; (add-hook 'kill-buffer-hook #'tm42-kill-buffer-hook)

(defmacro tm42/bg/debug-message (format-string &rest args)
  `(when tm42/bg/debug-tracing-enabled
    (message ,(concat "[tm42/bg] " format-string) ,@args)))

(defun tm42/bg/cyclic-nth (n &optional index lst)
  "Like nth, but cycles around when reaching the end of the list,
and negative n goes backwards."
  (unless index (setq index 0))
  (when lst
    (let* ((len (length lst))
           (new-index (mod (+ index n) len)))
      (if (< new-index 0)
          (nth (+ new-index len) lst)
        (nth new-index lst)))))

(defun tm42/bg/should-group-buffer (buffer-name)
  "Given buffer-name, determines whether that buffer should be grouped.
Useful for excluding things like minibuffers."
  (if (string-empty-p buffer-name)
      nil
    (if (or (= (aref buffer-name 0) ?\s))
        nil
      t)))

;;;; Footer

(provide 'tm42-buffer-groups)

;;; tm42-buffer-groups.el ends here

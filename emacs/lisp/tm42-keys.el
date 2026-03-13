;;; tm42-keys.el --- Keybindings                     -*- lexical-binding: t; -*-

;; Copyright (C) 2026

(require 'tm42-agenda)

(defun tm42/side-lock ()
  (interactive)
  (dolist (direction '(right bottom))
    (dolist (window (window-at-side-list nil direction))
      (set-window-parameter window 'no-other-window t))))

(defvar tm42/leader-map (make-sparse-keymap)
  "Top-level VM keymap under C-c.")

(define-key global-map (kbd "C-c") tm42/leader-map)

;; C-c b = buffer prefix
(defvar tm42/buffer-map (make-sparse-keymap))
(define-key tm42/leader-map (kbd "b") tm42/buffer-map)
(define-key tm42/buffer-map (kbd "r") #'revert-buffer)

;; C-c d = display prefix
(defvar tm42/display-map (make-sparse-keymap))
(define-key tm42/leader-map (kbd "d") tm42/display-map)

;; redraw display
(define-key tm42/display-map (kbd "r") #'redraw-display)

;; tabs
(defvar tm42/tab-map (make-sparse-keymap))
(define-key tm42/leader-map (kbd "t") tm42/tab-map)
(define-key tm42/tab-map (kbd "n") #'tab-bar-switch-to-next-tab)
(define-key tm42/tab-map (kbd "p") #'tab-bar-switch-to-prev-tab)
(define-key tm42/tab-map (kbd "k") #'tab-bar-close-tab)

;; sides
(defvar tm42/side-map (make-sparse-keymap))
(define-key tm42/display-map (kbd "s") tm42/side-map)
(define-key tm42/side-map (kbd "l") #'tm42/side-lock)
;; (define-key tm42/side-map (kbd "b") #'tm42/panel-toggle)
;; (define-key tm42/side-map (kbd "r") #'tm42/status-toggle)

;; C-c o = org-mode prefix
(defvar tm42/org-map (make-sparse-keymap))
(define-key tm42/leader-map (kbd "o") tm42/org-map)
(define-key tm42/org-map (kbd "c") #'org-capture)
(define-key tm42/org-map (kbd "p") #'org-set-property)
(defvar tm42/org-agenda-map (make-sparse-keymap))
(define-key tm42/org-map (kbd "a") tm42/org-agenda-map)
(define-key tm42/org-agenda-map "a" #'org-agenda)
(define-key tm42/org-agenda-map "d" #'tm42/agenda/toggle-daily-task)

;; C-c o = other prefix (focus/jump)
;; (defvar tm42/other-map (make-sparse-keymap))
;; (define-key tm42/leader-map (kbd "o") tm42/other-map)
;; (define-key tm42/other-map (kbd "o") #'other-window)
;; (define-key tm42/other-map (kbd "b") #'tm42/panel-focus)
;; (define-key tm42/other-map (kbd "r") #'tm42/status-focus)

;; C-c v = VC stuff
(defvar tm42/vc-map (make-sparse-keymap))
(define-key tm42/leader-map (kbd "v") tm42/vc-map)
(define-key tm42/vc-map (kbd "f") #'magit-file-dispatch)

(provide 'tm42-keys)

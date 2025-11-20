;;; tm42-display-rules.el --- Hybrid layout for interactive vs list buffers -*- lexical-binding: t; -*-

(defun tm42--side-bottom-config ()
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . 0.35)
    (window-parameters . ((no-other-window . t)))
    (inhibit-same-window . t)))

(defun tm42--project-eshell-interactive-display-rule-fix ()
  (with-eval-after-load 'project
    (defun tm42--project-eshell-bottom (orig &rest args)
      ;; Highest-precedence routing: beats display-buffer-same-window.
      (let ((display-buffer-overriding-action
             (tm42--side-bottom-config)))
        (apply orig args)))
    (advice-add 'project-eshell :around #'tm42--project-eshell-bottom)))

(defun tm42-install-interactive-display-rules ()
  (add-to-list
   'display-buffer-alist
   `((or (derived-mode . comint-mode)
         (derived-mode . eshell-mode)
         (derived-mode . compilation-mode)
         ("\\*compilation\\*" . 0)
         (derived-mode . grep-mode)
         ("\\*grep\\*" . 0))
     ,@(tm42--side-bottom-config)))
  (tm42--project-eshell-interactive-display-rule-fix))

;; (defun tm42--list-buffer-p (buffer _action)
;;   "Return non-nil if BUFFER is suited for the side sidebar."
;;   (with-current-buffer buffer
;;     (or
;;      (derived-mode-p 'compilation-mode)
;;      (derived-mode-p 'grep-mode))))

;; (defun tm42-install-list-display-rules ()
;;   (add-to-list
;;    'display-buffer-alist
;;    '(tm42--list-buffer-p
;;      (display-buffer-reuse-window display-buffer-in-side-window)
;;      (side . right)
;;      (slot . 0)
;;      (window-width . 0.33)
;;      (window-parameters . ((no-other-window . t)))
;;      (inhibit-same-window . t))))

(defun tm42-install-display-rules ()
  (setf switch-to-buffer-obey-display-actions t)
  (tm42-install-interactive-display-rules)
  ;;(tm42-install-list-display-rules)
  )

(defun tm42-install-display-rules-cleanly ()
  (setf display-buffer-alist '())
  (tm42-install-display-rules))

(defun tm42-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
     (not (window-dedicated-p (selected-window)))))

(provide 'tm42-display-rules)

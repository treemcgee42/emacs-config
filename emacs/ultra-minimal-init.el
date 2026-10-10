;; -*- lexical-binding: t; -*-

(let ((vendor-lisp-dir (expand-file-name "vendor-lisp" user-emacs-directory)))
  (when (file-directory-p vendor-lisp-dir)
    (add-to-list 'load-path vendor-lisp-dir)
    ;; System clipboard integration. In theory the builtin (and enabled) xterm
    ;; package should do this but I don't think it plays nice over SSH.
    (unless (display-graphic-p)
      (when (require 'clipetty nil t)
	(global-clipetty-mode 1)))))

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (menu-bar-mode -1))

(when (display-graphic-p)
  (pixel-scroll-precision-mode 1)
  (tool-bar-mode -1)
  (set-face-attribute 'default nil :height 180))
  

(load-theme 'modus-operandi t)
;; (load-theme 'modus-vivendi t)

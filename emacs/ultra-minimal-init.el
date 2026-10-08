
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
  (menu-bar-mode -1)
  (when (and (string= (tty-type) "xterm-ghostty") (not frame-background-mode))
    ;; Ghostty doesn't play well with background reporting. Setting xterm extras
    ;; doesn't seem to work. We're going to just guess...
    ;; https://github.com/ghostty-org/ghostty/discussions/5179
    ;; (customize-set-variable 'frame-background-mode 'dark)
    (customize-set-variable 'frame-background-mode 'light)))

(cond ((eql frame-background-mode 'light)
       (load-theme 'modus-operandi t))
      ((eql frame-background-mode 'dark)
       (load-theme 'modus-vivendi t)))

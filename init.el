(push "~/.config/emacs/vm" load-path)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(use-package magit)
(use-package winner)

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; [[ Company-specific ]]
;; These files are only included if they exist (I'll have them on company machines).

(condition-case nil
    (require 'vm-arista)
  (file-error nil))

;; [[ Misc ]]

(load-theme 'modus-vivendi)

;; Hide title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; Hides the buffer name and icon from the title bar.
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(scroll-bar-mode -1)

;; Makes it so the fringe doesn't have a special color, and blends in
;; with the background.
(set-face-attribute 'fringe nil :background nil)
(defun vm-remove-fringe-hook (frame)
  (select-frame frame)
  (set-face-attribute 'fringe nil :background nil))
(add-hook 'after-make-frame-functions 'vm-remove-fringe-hook)

(tool-bar-mode -1)

(if (not (display-graphic-p))
    (progn
      (menu-bar-mode -1)
      (xterm-mouse-mode 1)))

;; Saves window layouts. To return to previous window layout,
;; do C-c LEFT_ARROW.
(winner-mode 1)

(global-hl-line-mode 1)
(setq global-hl-line-sticky-flag t)

;;; [[ Multiple cursors ]]

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; [[ Keybindings ]]

;; SHIFT + <arrow key> to move to window
(windmove-default-keybindings)

;; [[ Common util functions ]]

(defun vm-kill-other-non-file-buffers ()
  "Close all other file buffers except the current one (e.g.
 preserve compilation, Messages buffers."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        ;; Only file buffers will have a non-nill buffer-file-name.
        (when (and (not (eq buffer current-buffer))
                   (buffer-file-name))
          (kill-buffer buffer))))))

(defun vm-extract-tramp-base (file)
  "Get the tramp prefix for the given file. If one is not found, the
empty string is returned.

E.g., given a file '/ssh:HOSTNAME:/foo/bar/baz.txt', it will
return '/ssh:HOSTNAME:'."
  (if (string-match "^/ssh:.*:" file)
      (match-string 0 file)
    ""))

(defun vm-get-parent-directory-name ()
  "Get the name of the parent directory of the current buffer's
 file.

E.g., a buffer for /src/Foo/bar.txt would return Foo."
  (let ((file-name (buffer-file-name)))
    (if file-name
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory file-name)))
      "nil")))

;; [[ Org mode ]]

(setq org-agenda-files '("~/org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)"  "|" "DONE(d!)")))

(defun vm-org-link-to-current-line ()
  "Creates an Org mode link to the file open in the current
 buffer, and the line pointed to by the cursor."
  (interactive)
  (let ((link (format "file:%s::%d" (buffer-file-name) (line-number-at-pos))))
    (kill-new link)
    (message "Copied '%s' to the clipboard" link)))

;; [[ Compilation ]]

;; Make the compilation buffer scroll with output.
(setq compilation-scroll-output t)

(defun vm-compilation-buffer-location-hook ()
  "Determines where the compilation buffer should pop up.

- If it's already visible, just use that.
- If there is only one window in the current frame, split it
  horizontally and use the newly created window.
- If there is more than one window, go to previously selected
  window, split it vertically, and use the newly created window."
  (message "Running compilation hook")
  ;; The first condition in the `and` checks if the buffer being
  ;; opened in compilation mode is exactly the usual compilation
  ;; buffer. This is necessary because, e.g., the grep buffer also
  ;; uses compilation mode.
  (when (and (string= "*compilation*" (buffer-name))
             (not (get-buffer-window "*compilation*")))
    (save-selected-window
      (save-excursion
        (cond ((eq (length (window-list)) 1)
               (let ((w (split-window-horizontally)))
                 (select-window w)
                 (switch-to-buffer "*compilation*")))
              (t
               (other-window 1)
               (let ((w (split-window-vertically)))
                 (select-window w)
                 (switch-to-buffer "*compilation*"))))))))
(add-hook 'compilation-mode-hook 'vm-compilation-buffer-location-hook)

;; [[ Grep ]]

(defun vm-grep-buffer-location-hook ()
  "Determines where the grep buffer should pop up.

- If it's already visible, just use that.
- If there is only one window in the current frame, split it
  horizontally and use the newly created window.
- If there is more than one window, go to previously selected
  window, split it vertically, and use the newly created window."
  (when (not (get-buffer-window "*grep*"))
    (cond ((eq (length (window-list)) 1)
           (let ((w (split-window-horizontally)))
             (select-window w)
             (switch-to-buffer "*grep*")))
          (t
           (other-window 1)
           (let ((w (split-window-vertically)))
             (select-window w)
             (switch-to-buffer "*grep*"))))))
(add-hook 'grep-mode-hook 'vm-grep-buffer-location-hook)

;; [[ Mode line ]]

(defun vm-update-mode-line-style ()
  "Updates styling to mode-line and mode-line-inactive."
  (set-face-attribute 'mode-line nil
                      :background "gray20"
                      :foreground "gray60"
                      :box '(:line-width 4 :color "gray20" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :background "gray40"
                      :foreground "gray80"
                      :box '(:line-width 4 :color "gray40" :style nil)))

(vm-update-mode-line-style)
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(add-hook 'after-load-theme-hook 'vm-update-mode-line-style)

(defface vm-mode-line-normal-face
  '((t))
  "Used for normal text."
  :group 'vm-mode-line-group)

(defface vm-mode-line-past-fill-column-face
  '((t
     :inherit 'vm-mode-line-normal-face
     :foreground "#ff4500"))
  "Changes the color of the column number when it exceeds the specified fill-column."
  :group 'vm-mode-line-group)

(defface vm-mode-line-saved-face
  '((t
     :inherit vm-mode-line-normal-face
     :foreground "#50C878"))
  "Color to indicate the working window is showing a saved buffer."
  :group 'vm-mode-line-group)

(defface vm-mode-line-unsaved-face
  '((t
     :inherit vm-mode-line-normal-face
     :foreground "#ff4500"))
  "Color to indicate the working window is showing an unsaved buffer."
  :group 'vm-mode-line-group)

(setq-default mode-line-format
	      '((:propertize " "
                             face vm-mode-line-normal-face)
                (:eval
		 (propertize "|||"
			     'face
			     (if (buffer-modified-p)
				 'vm-mode-line-unsaved-face
			       'vm-mode-line-saved-face)))
                (:propertize " "
                             face vm-mode-line-normal-face)
		(:propertize "%4l:"
			     face vm-mode-line-normal-face)
		(:eval
		 (propertize "%3c"
			     'face
			     (if (>= (current-column) fill-column)
				 'vm-mode-line-past-fill-column-face
			       'vm-mode-line-normal-face)))
		(:propertize " -- "
			     face vm-mode-line-normal-face)
		(:propertize "%b" face vm-mode-line-normal-face)
		)
	      )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(a4-enable-default-bindings nil)
 '(column-number-mode t)
 '(grep-command "rg -nS --no-heading \"\"")
 '(grep-command-position 22)
 '(indent-tabs-mode nil)
 '(org-log-into-drawer t)
 '(package-selected-packages
   '(multiple-cursors zig-mode orderless consult marginalia vertico vterm xcscope magit))
 '(scroll-preserve-screen-position 1)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal)))))

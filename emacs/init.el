(defmacro measure-time (msg &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06f"  ,msg (float-time (time-since time)))))

(push "~/.config/emacs/vm" load-path)
(push "~/.config/emacs/lisp" load-path)
(push "~/.config/emacs/themes" load-path)

;; Install elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Have shells, shell commands, etc. use emacs itself as an editor for
;; commands that need an editor (like git commit).
(use-package with-editor
  :ensure t
  :commands (with-editor-export-editor with-editor-shell-command with-editor-async-shell-command)
  :bind (([remap shell-command]       . with-editor-shell-command)
         ([remap async-shell-command] . with-editor-async-shell-command))
  :config (dolist (hook '(shell-mode-hook term-exec-hook eshell-mode-hook))
            (dolist (envvar with-editor-envvars)
              (add-hook hook (apply-partially 'with-editor-export-editor envvar)))))

;; Magit requires a higher version of seq then installed on older emacs versions,
;; this workaround is from https://github.com/progfolio/elpaca/issues/216
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))
(elpaca `(seq :build ,(+elpaca-seq-build-steps)))
(use-package magit
  :after seq
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-flat
  :after vertico
  :ensure nil
  :init (vertico-flat-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-g i" . consult-imenu))
  :custom
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-\\" . er/expand-region)))

;; Colors in compilation buffer.
(use-package ansi-color
  :ensure nil ; included in emacs
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Clipboard access for terminal emacs / tmux. I didn't actually need this when
;; using zellij...
(unless (display-graphic-p)
  (use-package clipetty
    :ensure t
    :hook (after-init . global-clipetty-mode)) ;; todo remove
  (add-hook
   'elpaca-after-init-hook
   #'global-clipetty-mode))

(use-package eat
  :ensure t
  :config (progn
           (push (vector meta-prefix-char ?o) eat-semi-char-non-bound-keys)
           (eat-update-semi-char-mode-map)
           (eat-reload))
  :config
  ;; Ideally we would use :hook here, but for some reason it causes an infinite
  ;; loop when loading eshell. This is a workaround.
  (with-eval-after-load 'eshell
    (eat-eshell-mode +1)
    (eat-eshell-visual-command-mode +1)))

(use-package rg
  :ensure t)

;; [[ Company-specific ]]
;; These files are only included if they exist (I'll have them on company machines).

(measure-time
 "Loading vm-arista"
 (condition-case nil
     (require 'vm-arista)
   (file-error nil)))

;; [[ Misc ]]

(setq inhibit-startup-screen t)
;; (load-theme 'modus-vivendi)
(load-theme 'minimal-tron t)
;; (disable-theme 'modus-vivendi)

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
  ;; Removes the fringe for a single frame
  (select-frame frame)
  (set-face-attribute 'fringe nil :background 'unspecified))
(defun vm-remove-fringe-hook-all-frames (&rest args)
  ;; Removes the fringe for all frames.
  (dolist (frame (frame-list))
    (vm-remove-fringe-hook frame)))
(add-hook 'after-make-frame-functions 'vm-remove-fringe-hook)
(advice-add 'load-theme
            :after #'vm-remove-fringe-hook-all-frames)

(tool-bar-mode -1)

(if (not (display-graphic-p))
    (progn
      (menu-bar-mode -1)
      (xterm-mouse-mode 1)))

(global-hl-line-mode 1)
(setq global-hl-line-sticky-flag t)

;;; [[ Movement ]]

(global-set-key (kbd "<f1>") 'previous-buffer)
(global-set-key (kbd "<f2>") 'next-buffer)
(global-set-key (kbd "<f3>") 'other-window)

(global-set-key (kbd "C-x ]") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-x [") 'tab-bar-switch-to-prev-tab)

(global-set-key (kbd "M-n")
                (lambda ()
                  (interactive)
                  (forward-line)
                  (scroll-up-line)))
(global-set-key (kbd "M-p")
                (lambda ()
                  (interactive)
                  (previous-line)
                  (scroll-down-line)))

;; For move-text to re-indent line when moved.
;; From @jbreeden on the move-text Github README.
(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

;; M-up / M-down to move the current line up / down.
(use-package move-text
  :ensure t
  :config
  (progn
    (move-text-default-bindings)
    (advice-add 'move-text-up :after 'indent-region-advice)
    (advice-add 'move-text-down :after 'indent-region-advice)))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  (global-set-key (kbd "C-c ;") 'avy-goto-char-timer))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))
(add-hook
 'elpaca-after-init-hook
 (lambda ()
   (setq switch-to-buffer-obey-display-actions t)
   (global-set-key (kbd "C-x 4 o") 'ace-window-prefix)))
(defun ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(defun tm42-smart-beginning-of-line ()
  "Move point to first beginning of line or non-whitespace character.
If the point is already at the beginning of the line, move to the
first non-whitespace character. Otherwise just move to the beginning
of the line."
  (interactive)
  (let ((original-point (point)))
    (move-beginning-of-line 1)
    (when (= original-point (point))
      (back-to-indentation))))
(global-set-key (kbd "C-a") 'tm42-smart-beginning-of-line)

;; From Karthinks emacs window management almanac
(define-advice pop-global-mark (:around (pgm) use-display-buffer)
  "Make `pop-to-buffer' jump buffers via `display-buffer'."
  (cl-letf (((symbol-function 'switch-to-buffer)
                         #'pop-to-buffer))
                (funcall pgm)))

;; [[ Keybindings ]]

;; SHIFT + <arrow key> to move to window
(windmove-default-keybindings)

;; [[ Common util functions ]]

(defun tm42-insert-into-list (l obj n)
  "Insert into list LIST an element EL at index N.

If N is 0, EL is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid.

https://stackoverflow.com/questions/20821295/how-can-i-insert-into-the-middle-of-a-list-in-elisp"
  (let* ((padded-list (cons nil l))
         (c (nthcdr n padded-list)))
    (setcdr c (cons obj (cdr c)))
    (cdr padded-list)))

(defun tm42-cyclic-nth (n &optional index lst)
  "Like nth, but cycles around when reaching the end of the list,
and negative n goes backwards."
  (unless index (setq index 0))
  (when lst
    (let* ((len (length lst))
           (new-index (mod (+ index n) len)))
      (if (< new-index 0)
          (nth (+ new-index len) lst)
        (nth new-index lst)))))

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
  "Determines where the compilation buffer should pop up."
  (message "Running compilation hook")
  ;; The first condition in the `and` checks if the buffer being
  ;; opened in compilation mode is exactly the usual compilation
  ;; buffer. This is necessary because, e.g., the grep buffer also
  ;; uses compilation mode.
  (when (and (string= "*compilation*" (buffer-name))
             (not (get-buffer-window "*compilation*"))
             (not display-buffer-overriding-action))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w))
               (compilation-window-height 10))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'vm-compilation-buffer-location-hook)

(defun tm42-switch-to-compilation-buffer-on-failure (buffer msg)
  "Switch to the compilation buffer if the compilation fails."
  ; This is admittedly not the best heuristic to detect compilation
  ; failure.
  (unless (string-match "finished" msg)
    (let ((buffer-win (get-buffer-window buffer t)))
      (when buffer-win
        (select-window buffer-win)
        (goto-char (point-min))
        (compilation-next-error 1)))))

(add-hook 'compilation-finish-functions
          'tm42-switch-to-compilation-buffer-on-failure)

;; [[ Completion ]]

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :custom
    (corfu-terminal-mode 1)))

(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-hook 'completion-at-point-functions #'cape-history)
  ;;(add-hook 'completion-at-point-functions #'cape-keyword)
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-hook 'completion-at-point-functions #'cape-line)
)
  
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

;; [[ Buffer groups ]]

;; (use-package tm42-buffer-groups
;;   :ensure nil
;;   :demand
;;   :bind (("C-x <left>" . tm42/bg/previous-buffer)
;;          ("C-x <right>" . tm42/bg/next-buffer)))

;; [[ Mode line ]]

(defface tm42/ml/normal-face
  '((t))
  "Used for normal text."
  :group 'tm42/ml/group)
(defface tm42/ml/bold-face
  '((t
     :inherit 'tm42/ml/normal-face
     :weight bold))
  "Just a bold face."
  :group 'tm42/ml/group)
(defface tm42/ml/past-fill-column-face
  '((t
     :inherit 'tm42/ml/normal-face
     :foreground "#ff4500"))
  "Changes the color of the column number when it exceeds the specified fill-column."
  :group 'tm42/ml/group)
(defface tm42/ml/saved-face
  '((t
     :inherit tm42/ml/normal-face
     :foreground "#50C878"))
  "Color to indicate the working window is showing a saved buffer."
  :group 'tm42/ml/group)
(defface tm42/ml/unsaved-face
  '((t
     :inherit tm42/ml/normal-face
     :foreground "#ff4500"))
  "Color to indicate the working window is showing an unsaved buffer."
  :group 'tm42/ml/group)

(setq tm42/ml/right-aligned-content
      '(""
        mode-line-misc-info))

;; From Tyler Grinn
;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline
(defun tm42/ml/padding-before-right-aligned-content ()
  (let ((r-length (length (format-mode-line tm42/ml/right-aligned-content))))
    (propertize " "
                'display
                `(space :align-to (- right ,r-length)))))

(defun tm42/ml/mode-line ()
  (interactive)
  (setq-default mode-line-format
	        '((:propertize " "
                               face tm42/ml/normal-face)
                  (:eval
                   (if (buffer-modified-p)
                       (propertize "%p"
                                   'face
                                   'tm42/ml/unsaved-face)
                     (propertize "%p"
                                 'face
                                 'tm42/ml/saved-face)))
                  (:propertize " "
                               face tm42/ml/normal-face)
		  (:propertize "%4l:"
			       face tm42/ml/normal-face)
		  (:eval
		   (propertize "%3c"
			       'face
			       (if (>= (current-column) fill-column)
				   'tm42/ml/past-fill-column-face
			         'tm42/ml/normal-face)))
		  (:propertize "    "
			       face tm42/ml/normal-face)
		  (:propertize "%b" face tm42/ml/bold-face)
                  (:eval (tm42/ml/padding-before-right-aligned-content))
                  (:eval tm42/ml/right-aligned-content)
		  )
	        ))

(defun tm42/ml/default-mode-line ()
  (interactive)
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (:propertize
                   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                   display
                   (min-width (5.0)))
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces)))

(which-function-mode)
(tm42/ml/mode-line)

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             0.5
             nil
             (lambda ()
               (message "Emacs started in %s" (emacs-init-time))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(a4-enable-default-bindings nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("c43813c439df1a3d5d373b7d91f628caffcb4ecdb908e562200a7b061e6e4cbc" "b1b5502bc64071b2e263ecfcb01bd1d784497c795c61c2cb6de4a3f459a91da9" "4c22e0a991f91a6a166699a8f267112934478db6592977c74926173f7f6c8071" "6072798c95eeda3719f455590248f2a641c440418cad564f0853d91ad7641944" "724ec1789ab57edf52040cf39280c0e09e2a8f0b0556695569e5ba3986fc183d" "6cd3c963db7aa40c9c0abf5caa923c4205a885fe583f4fd1c33e722b3535a763" "0b08daef5c9b853c1bf82a0797bcd8d4d333be2dbe7aba402064a9653196991d" "d5b6892dabfa54f659918326e459dcc3f4851724759063c1ff2c3e43c734b6cb" "2d405365e4edaf423465d961b8bcc09f4d280af05ab0319924f7017c1fcf136d" "3a65dffab04340599fb2daf6e8db5349f65b9c0403a3b98b5927442ca97c16b1" "4ce77ae7163893c4dd8629d00aa7a26013b680e4be59021e2d2a80544ab34939" "998c811d828dbc02eff645e633dfcc90e02ebdad9558a457e622be1335de211b" "19aca151c4a38aefee68109e6701d2555fadd987cbf12e7d90b5af4e66d89548" "6538d61c331f93f2089e5e53141798ca954e2dd4eb43e773b288c0d15ffc8d6c" "fe08a51edfb96058e164f2c013a2b17a6114aaf6c6dc7fc6ce28df4736a46c83" "6a18e904da7918d42a6d9bc1d6936b13fc48763e3dc87e0df87a3ed893b6b7b8" "fa09c11029549fc9ae9088772034aae80ec3d91c25b09e58f23a2ae30435406d" "c6a3b79fbe9462a6f057d941a959e71d3945158424fc05ec46204d58e2d182a0" "821c37a78c8ddf7d0e70f0a7ca44d96255da54e613aa82ff861fe5942d3f1efc" "835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f" default))
 '(grep-command "rg -nS --no-heading \"\"")
 '(grep-command-position 22)
 '(indent-tabs-mode nil)
 '(org-log-into-drawer t)
 '(package-selected-packages
   '(markdown-mode vlf font-lock-studio cape corfu-terminal corfu clipetty rg acme-theme ace-window git-gutter tm42-buffer-groups expand-region org-roam avy move-text multiple-cursors zig-mode orderless consult marginalia vertico vterm xcscope magit))
 '(scroll-margin 3)
 '(scroll-preserve-screen-position 1)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal)))))
(put 'erase-buffer 'disabled nil)

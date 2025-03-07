;;; init.el --- My init file ---        -*- lexical-binding: t; -*-

(message "top of init file")

(defmacro tm42/init-section (name &rest body)
  (declare (indent 1))
  `(let ((start-time (current-time)))
     (message "init section %s punched IN at %s"
              ,name
              (format-time-string "%H:%M:%S" start-time))
     (progn ,@body)
     (let ((end-time (current-time)))
       (message "init section %s punched OUT at %s (took %.06f)"
                ,name
                (format-time-string "%H:%M:%S" end-time)
                (float-time (time-subtract end-time start-time))))))

(defmacro measure-time (msg &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06f"  ,msg (float-time (time-since time)))))

(push "~/.config/emacs/vm" load-path)
(push "~/.config/emacs/lisp" load-path)
(push "~/.config/emacs/themes" load-path)


;; Install Elpaca
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

(tm42/init-section "Magit"
  ;; Magit requires a higher version of seq then installed on older emacs versions,
  ;; this workaround is from https://github.com/progfolio/elpaca/issues/216
  (defun +elpaca-unload-seq (e)
    (and (featurep 'seq) (unload-feature 'seq t))
    (elpaca--continue-build e))
  (defun +elpaca-seq-build-steps ()
    (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
			 elpaca--pre-built-steps elpaca-build-steps))
            (list '+elpaca-unload-seq 'elpaca--activate-package)))
  ;; (elpaca `(seq :build ,(+elpaca-seq-build-steps)))
  (use-package magit
    :after seq
    :ensure t))

(customize-set-value 'dabbrev-case-replace nil)

(when (locate-library "completion-preview")
  (with-eval-after-load 'completion-preview
    (customize-set-value 'completion-preview-idle-delay 3)
    (add-hook 'prog-mode-hook #'completion-preview-mode)))

(use-package cape
  :ensure t
  :init
  ;; The order of the functions matters, the first function returning a result wins.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; --- begin consult ----------------------------------------------------------------

(tm42/init-section "Consult"
  (use-package consult
    :ensure t
    :bind (("C-x b" . consult-buffer)
           ("C-x p b" . consult-project-buffer)
           ("C-x p f" . consult-fd)
           ("M-g i" . consult-imenu)
           ("M-g g" . consult-goto-line)
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register)
           ("M-'" . consult-register-store))          ;; orig. abbrev-prefix-mark (unrelated)
    :custom
    ;; Tweak the register preview for `consult-register-load',
    ;; `consult-register-store' and the built-in commands.  This improves the
    ;; register formatting, adds thin separator lines, register sorting and hides
    ;; the window mode line.
    (advice-add #'register-preview :override #'consult-register-window)
    (setq register-preview-delay 0.5)
    (xref-show-xrefs-function #'consult-xref)
    (xref-show-definitions-function #'consult-xref)
    (completion-in-region-function #'consult-completion-in-region))
  (with-eval-after-load 'em-hist
    (bind-key "M-r" #'consult-history 'eshell-hist-mode-map))

  (defun tm42/pdb-locations ()
    "Return value is a hash map whose key is a descriptive string and whose value
is an alist with the following keys:
- MARKER: the value of the point as a marker object
- LINE
- COLUMN
"
    (let ((result (make-hash-table :test 'equal)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (rx ".pdb()") nil t)
          (let ((line (line-number-at-pos))
                (column (current-column))
                (marker (point-marker)))
            (puthash
             (format "Line %d column %s" line column)
             (list (cons 'line line)
                   (cons 'column column)
                   (cons 'marker marker))
             result))))
      result))


  (defun tm42/consult-breakpoints ()
    "View the breakpoints in the current buffer in an imenu-like interface.
This function utilizes consult."
    (interactive)
    (let ((locations (tm42/pdb-locations)))
      (consult--read
       locations
       :prompt "Breakpoint: "
       :category 'consult-location
       :state
       (let ((preview (consult--jump-state)))
         (lambda (action candidate)
           (funcall preview
                    action
                    (when candidate
                      (alist-get 'marker
                                 (gethash candidate locations))))))
       :require-match t))))

;; --- end consult ------------------------------------------------------------------

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(fido-mode)
(customize-set-value
 'icomplete-prospects-height 1
 "Do not expand the minibuffer with extra search results. I find that to be distracting.")
(defun tm42/icomplete-styles ()
  (setq-local completion-styles '(orderless flex)))
(add-hook 'icomplete-minibuffer-setup-hook 'tm42/icomplete-styles)
(define-key global-map (kbd "C-M-v") 'switch-to-completions)
(define-key icomplete-fido-mode-map (kbd "TAB") #'icomplete-force-complete)

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
            (push (vector meta-prefix-char ?#) eat-semi-char-non-bound-keys)
            (eat-update-semi-char-mode-map)
            (eat-reload)))

(use-package rg
  :ensure t)

;; [[ Eshell ]]

(with-eval-after-load 'em-banner
  (customize-set-value 'eshell-banner-message ";
;     |\\_._/|
;     | 0 0 |
;     (  T  )    Are we consing yet?
;    .^`-^-'^.
;    `.  ;  .'
;    | | | | |
;   ((_((|))_))
;             hj
;
; Welcome to the Emacs shell.\n\n"))

(defun eshell/comp (&rest args)
  "Calls `compile' interactively with the remaining arguments as the command."
  (compile (string-join args " ") t))

(defun eshell/out (&rest args)
  "Run a shell command asynchronously and show its output in a temporary buffer.

The output appears in the current window. Press `q` to quit the buffer and return to Eshell.
A message will also be printed in the buffer when the command completes."
  (let* ((buf-name "*eshell-output*")
         (cmd (string-join args " "))
         (default-directory (eshell/pwd)))
      (tm42/compile-to-buffer cmd buf-name nil nil t)))

(defun eshell/tm42-copy (&optional n)
  "Copy the last N eshell command and command outputs."
  (let ((start nil)
        (end nil))
    (save-excursion
      (eshell-previous-prompt n)
      (beginning-of-line)
      (setf start (point))
      ;; We are currently at the beginning of the line of a prompt. Going to the next
      ;; prompt now will move to the end of the prompt on this line.
      ;; `eshell-end-of-output' will move to the end of the output of the current
      ;; prompt.
      (eshell-next-prompt n)
      (setf end (eshell-end-of-output))
      (copy-region-as-kill start end))))

(defun tm42/eshell-redirect-to-temp-buffer (&rest args)
  "Run ARGS as a command and redirect output to a temp buffer named *eshell-CMD*."
  (let* ((cmd-name (car args))
         (cmd-string (mapconcat #'identity args " "))
         (buf-name (format "*eshell-%s*" cmd-name)))
    (eshell-command (format "%s > #<buffer %s>" cmd-string buf-name))
    (switch-to-buffer-other-window buf-name)))

(defalias 'et 'tm42/eshell-redirect-to-temp-buffer)

(setq eshell-prompt-function
      (lambda ()
        (concat
         (format-time-string "[%m/%d %H:%M] " (current-time))
         (abbreviate-file-name (eshell/pwd))
         " $ ")))

(with-eval-after-load 'eshell
  (require 'em-smart)
  (add-to-list 'eshell-modules-list 'eshell-smart))
  ;; (add-to-list 'eshell-visual-commands "watch"))

;; --- begin eshell smart restore command -------------------------------------------

(defvar *tm42/eshell/prev-cmd* ""
  "Stores the previously executed eshell command, for the restore command
functionality.")

(defun tm42/eshell/restore-prev-cmd-p ()
  "Function to determine whether we should be exercising the restore command
functionality."
  (and (member 'eshell-smart eshell-modules-list)))

(defun tm42/eshell/get-input ()
  "Get the input at the current eshell prompt. Assumes point is within the input."
  (let ((beg (save-excursion
               (eshell-previous-prompt 0)
               (point)))
        (end (save-excursion
               (end-of-line)
               (point))))
    (buffer-substring-no-properties beg end)))

(defun tm42/eshell/maybe-restore-prev-cmd (&optional use-region queue-p no-newline)
  "In eshell smart mode, when modifying the previous command, calling this function
before `eshell-send-input' (the function RET is bound to) will restore the previous
command to the prompt line. That way, the output of the previous command will
correspond to the input on the prompt above it."
  (when (and (tm42/eshell/restore-prev-cmd-p)
             *tm42/eshell/prev-cmd*)
    (end-of-line)
    (when (not (eql (point) (point-max)))
      (let ((current-cmd (tm42/eshell/get-input)))
        (eshell-previous-prompt 0)
        (kill-line)
        (insert *tm42/eshell/prev-cmd*)
        (goto-char (point-max))
        (insert current-cmd)))))

(defun tm42/eshell/store-prev-cmd (&optional use-region queue-p no-newline)
  "Store the command that was just executed, assuming eshell smart mode."
  (when (tm42/eshell/restore-prev-cmd-p)
    (setf *tm42/eshell/prev-cmd* (tm42/eshell/get-input))))

(with-eval-after-load 'eshell
  (advice-add 'eshell-send-input :before #'tm42/eshell/maybe-restore-prev-cmd)
  (advice-add 'eshell-send-input :after #'tm42/eshell/store-prev-cmd))

;; --- end eshell smart restore command ---------------------------------------------

;; [[ Misc ]]

(setq inhibit-startup-screen t)
;; (load-theme 'modus-vivendi)
(load-theme 'minimal-tron-light t)
;; (disable-theme 'modus-vivendi)

;; Hide title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; Hides the buffer name and icon from the title bar.
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

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

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'project-try-vc))

(defun tm42/vc-quick-commit ()
  (interactive)
  (let ((dir (vc-root-dir)))
    (vc-dir dir)
    ;; Buffer name logic ripped from the vc-dir source code (:
    (with-current-buffer (vc-dir-prepare-status-buffer
                          "*vc-dir*" dir (vc-responsible-backend dir))
      (vc-dir-mark-state-files '(edited))
      (vc-diff))))

(defun tm42/diff-mode-toggle-subtree ()
  "Toggle the visibility of the current diff hunk or file in `diff-mode`."
  (interactive)
  (if (outline-invisible-p (line-end-position))
      (outline-show-subtree)
    (outline-hide-subtree)))

(with-eval-after-load 'diff-mode
  (define-key diff-mode-shared-map (kbd "TAB") #'tm42/diff-mode-toggle-subtree))

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; --- Tab bar ---------------------------------------------------------------------

(tool-bar-mode -1)

(if (not (display-graphic-p))
    (progn
      (menu-bar-mode -1)
      (xterm-mouse-mode 1)))

(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))

;; Honestly the menu bar is still useful-- combine it with the tab-bar.

(require 'tab-bar)
(tab-bar-mode)

(defun tm42/tab-bar-compilation-provider-generator (buffer-rx text)
  "Helper to generate functions for `tm42/tab-bar-compilation-status-providers'.
It generates a function that checks how many buffers have a name matching
BUFFER-RX and also have an active process. TEXT is the text that will be
displayed in the tab bar."
  (lambda ()
    (let ((buffers
           (seq-filter (lambda (buf)
                         (and (string-match buffer-rx (buffer-name buf))
                              (get-buffer-process buf)))
                       (buffer-list))))
      (cons text (length buffers)))))

(defvar tm42/tab-bar-compilation-provider
  (tm42/tab-bar-compilation-provider-generator
   "compilation"
   "Compiling"))

(defvar tm42/tab-bar-compilation-status-providers
  (list tm42/tab-bar-compilation-provider)
  "Functions which may provide information to populate the tab bar
compilation status.  Each function should take no arguments and
return a cons cell whose car is the name to be displayed and cdr
is the number of instances. The name is displayed if the number
of instances is nonzero, and the number of instances is displayed
if it is greater than one.")

(defun tm42/tab-bar-compilation-status ()
  (let (items)
    (dolist (provider tm42/tab-bar-compilation-status-providers)
      (pcase-let ((`(,text . ,num) (funcall provider)))
        (when (> num 0)
          (if (> num 1)
              (push (format "%s(%d)" text num) items)
            (push text items)))))
    (when items
      (concat "[" (mapconcat #'identity items ", ") "]"))))

(defvar tm42/workspace-name nil
  "Name of the workspace or session. At work I have multiple Emacs instances running
at once, so it's useful to have an easy way to tell which is which.")
(defun tm42/set-workspace-name (name)
  (interactive "sWorkspace name: ")
  (setf tm42/workspace-name name))

(setq tab-bar-format
      (list #'tab-bar-format-menu-bar
            #'tab-bar-format-history
            #'tab-bar-format-tabs
            #'tab-bar-separator
            #'tab-bar-format-add-tab
            #'tab-bar-format-align-right
            #'tm42/tab-bar-compilation-status
            (lambda ()
              (when tm42/workspace-name
                (format "  [Workspace: %s]  " tm42/workspace-name)))
            #'tab-bar-format-global))
(add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)
(setq tab-bar-menu-bar-button " π ")
;; Clock
(setenv "TZ" "America/New_York")
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %e %b %T")
(setq display-time-interval 1)
;; (display-time-mode -1)

(defun tab-bar-tab-name-format-comfortable (tab i) (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ") 'face (funcall tab-bar-tab-face-function tab)))
(setq tab-bar-tab-name-format-function #'tab-bar-tab-name-format-comfortable)

;; --- End tab bar -----------------------------------------------------------------

(use-package embark
  :ensure t

  :bind
  (("C-c ." . embark-act)         ;; pick some comfortable binding
   ("C-c ;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :ensure t)
(with-eval-after-load 'which-key
  (which-key-mode))

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun disable-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))
(add-hook 'eshell-mode-hook
          'disable-show-trailing-whitespace)
(add-hook 'compilation-mode-hook
          'disable-show-trailing-whitespace)
(add-hook 'comint-mode-hook
          'disable-show-trailing-whitespace)

(message "end section MISC")

;; --- Languages -------------------------------------------------------------------
(message "begin section LANGUAGES")

(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (python "https://github.com/tree-sitter/py-tree-sitter")))

(when (treesit-available-p)
  (push '("\\.py\\'" . python-ts-mode) auto-mode-alist))

(defun tm42/lineup-arglist-intro (langelem)
  "Indent the first argument relative to the previous line."
  (message "HERE")
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    ;; (forward-line -1)
    (message "at line %d current indentation %d" (line-number-at-pos) (current-indentation))
    (+ (current-indentation) c-basic-offset)))

(c-add-style
 "tm42"
 '((c-basic-offset . 4)
   (c-offsets-alist
    (arglist-intro add [0] tm42/lineup-arglist-intro)
    (arglist-close . c-lineup-arglist)
    (innamespace . [0]))))

(defun tm42/c-mode-hook ()
  (c-set-style "tm42")
  (setq-local c-basic-offset 4))

(use-package cc-mode
  :init
  (add-hook 'c++-mode-hook 'tm42/c-mode-hook))

(message "end section LANGUAGES")
;; --- End languages ---------------------------------------------------------------

;; --- begin Movement ---------------------------------------------------------------
(tm42/init-section
 "Movement"
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

 (repeat-mode 1)

 (defun tm42/move-and-scroll (distance)
   (forward-line distance))

 (defun scroll-half-page-down ()
   "scroll down half the page"
   (interactive)
   (let ((distance (/ (window-body-height) 2)))
     (tm42/move-and-scroll distance)))

 (defun scroll-half-page-up ()
   "scroll up half the page"
   (interactive)
   (let ((distance (/ (window-body-height) 2)))
     (tm42/move-and-scroll (* -1 distance))))

 (global-set-key (kbd "C-v") 'scroll-half-page-down)
 (global-set-key (kbd "M-v") 'scroll-half-page-up)

 ;; (require 'view)
 ;; (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
 ;; (global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

 (use-package tm42-maximize-window
   :ensure nil
   :bind (([mode-line mouse-3] . 'tm42/maximize-clicked-window)))

 (use-package tm42-compilation-status
   :ensure nil)

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
   (progn
     (unbind-key "C-c ;")
     (global-set-key (kbd "C-c ; c") 'avy-goto-char-timer)
     (global-set-key (kbd "C-c ; l") 'avy-goto-line)))

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

 ;; Use selection to search.
 ;; https://blog.chmouel.com/posts/emacs-isearch/
 (defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
   (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
       (progn
         (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
         (deactivate-mark)
         ad-do-it
         (if (not forward)
             (isearch-repeat-backward)
           (goto-char (mark))
           (isearch-repeat-forward)))
     ad-do-it))
 )
;; --- end Movement -----------------------------------------------------------------

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

(with-eval-after-load 'org
  (defun get-org-agenda-files ()
    (let* ((project-umbrella-dir "~/dev/projects")
           (project-dirs
            (seq-filter #'file-directory-p
                        (directory-files project-umbrella-dir t directory-files-no-dot-files-regexp t)))
           (top-level-project-org-files
            (apply #'append
                   (mapcar (lambda (dir)
                             (directory-files dir t "\\.org$" t))
                           project-dirs))))
      (append '("~/dev/projects/todo.org") top-level-project-org-files)))
  (setq org-agenda-files (get-org-agenda-files))

  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "STARTED(s!)"
                    "YOUR TURN(y!)"
                    "THEIR TURN(w!)"
                    "|" ; Completed states
                    "DONE(d!)")))

  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/dev/projects/todo.org")
           "* TODO %?")))

  ;; Default keybinding C-c C-, is not recognized in the terminal.
  (define-key org-mode-map (kbd "C-c ,") #'org-insert-structure-template)
  ;; C-c ; is used by avy.
  (define-key org-mode-map (kbd "C-c ;") nil)

  (defun tm42/org-link-to-fn ()
    (interactive)
    (save-excursion
      (beginning-of-defun)
      (org-store-link 0 t)
      (let ((link (car (car org-stored-links)))
            (desc (format "~%s~" (which-function))))
        (message "link %s desc %s" link desc)
        (kill-new (org-link-make-string link desc))
        (setq org-stored-links (cdr org-stored-links))
        (message "Copied link to keyboard. Isn't that nice?"))))

  (defun vm-org-link-to-current-line ()
    "Creates an Org mode link to the file open in the current
 buffer, and the line pointed to by the cursor."
    (interactive)
    (let ((link (format "file:%s::%d" (buffer-file-name) (line-number-at-pos))))
      (kill-new link)
      (message "Copied '%s' to the clipboard" link)))

  (require 'ox-md)
  (defun tm42/org-copy-region-as-markdown ()
    "Copy the region (in Org) to the system clipboard as Markdown."
    (interactive)
    (when (use-region-p)
      (let* ((region (buffer-substring-no-properties
		      (region-beginning)
		      (region-end)))
	     (markdown (org-export-string-as
                        region 'md t '(:with-toc nil))))
        (kill-new markdown))))
  )

;; [[ dumb grep ]]

(use-package tm42-dumbgrep
  :ensure nil)

;; [[ Compilation ]]

;; Make the compilation buffer scroll with output.
(setq compilation-scroll-output t)

(defun tm42/set-compilation-scrollback (size)
  (interactive "nScrollback size: ")
  (add-hook 'compilation-filter-hook 'comint-truncate-buffer)
  (setq comint-buffer-maximum-size size))

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

(add-hook 'compilation-mode-hook
          (lambda () (setq truncate-lines t)))
(defun tm42/untruncate-lines (buffer msg)
  (unless (string-match "finished" msg)
    (toggle-truncate-lines 0)))
(add-hook 'compilation-finish-functions 'tm42/untruncate-lines)

(defun tm42/compile-to-buffer (command &optional buf comint ask use-current-window)
  "Wrapper around `compile'. COMMAND is the command to execute. BUF, if non-nil, is
the name of the buffer to compile to. COMINT controls interactive compilation. ASK
indicates whether the user should be prompted with the command and required to
confirm before proceeding (similar to what happens when calling `compile'
interactively)."
  (cl-letf (;; Optionally override the function that determines the compilation
            ;; buffer name.
            ((symbol-function 'compilation-buffer-name)
             (if buf
                 (lambda (name-of-mode _mode-command name-function)
                   buf)
               #'compilation-buffer-name))
            ;; When calling interactively this is used since we can't directly pass
            ;; in a comint argument. The value must be a cons, looking at the
            ;; implementation.
            (current-prefix-arg (when comint '(1)))
            ;; When calling interactively this will fill in the default command
            ;; for the user to verify.
            (compile-command command)
            (display-buffer-overriding-action
             (when use-current-window
             '((display-buffer-reuse-window display-buffer-same-window)))))
    (if ask
        (call-interactively #'compile)
      (compile command comint))))

(defun tm42/pdb-to-buffer (command &optional ask)
  (if ask
      (progn
        (push command gud-pdb-history)
        (call-interactively #'pdb))
    (pdb command)))

(defun tm42/send-text-to-comint (text comint-buffer-name)
  (comint-send-string comint-buffer-name text)
  (comint-send-string comint-buffer-name "\n"))

(defun tm42/send-region-to-comint (start end comint-buffer-name)
  (interactive "r\nbComint buffer: ")
  (let ((text (buffer-substring-no-properties start end)))
    (tm42/send-text-to-comint text comint-buffer-name)))

(defun tm42/compile-goto-error-this-window (&optional event)
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'compile-goto-error)))

(defun tm42/compile-goto-error-other-window (&optional event)
  (interactive)
  (compile-goto-error event))

(defvar tm42/grep-mode-map (make-sparse-keymap))
(define-key tm42/grep-mode-map (kbd "O") #'tm42/compile-goto-error-this-window)
(define-key tm42/grep-mode-map (kbd "o") #'tm42/compile-goto-error-other-window)

(define-minor-mode tm42/grep-mode
  "Minor mode to add tm42's customizations to grep-mode."
  :keymap tm42/grep-mode-map)

(with-eval-after-load 'grep
  (add-hook 'grep-mode-hook #'tm42/grep-mode))

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

(use-package tm42-scrolling-elt
  :ensure nil)

(setq tm42/ml/right-aligned-content
      '(""
        which-func-format))

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
                  "   "
                  ;; (vc-mode vc-mode)
                  (:eval
                   (progn
                     (when (not (local-variable-p 'tm42/vc-ml-scrolling-elt))
                       (setq-local
                        tm42/vc-ml-scrolling-elt
                        (tm42/make-scrolling-modeline-elt
                         (lambda ()
                           (when (and (boundp 'vc-mode) vc-mode)
                             (string-trim (substring-no-properties vc-mode))))
                         12)))
                     (tm42/tick-scrolling-modeline-elt tm42/vc-ml-scrolling-elt)))
                  (:eval (tm42/ml/padding-before-right-aligned-content))
                  (:eval tm42/ml/right-aligned-content)
		  )
	        ))

(defun tm42/mode-line-2 ()
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
                  "   "
                  which-func-format
                  "   "
                  "(" (vc-mode vc-mode) " )"
                  mode-line-end-spaces)))

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
                  which-func-format
                  mode-line-end-spaces)))

(which-function-mode)
(tm42/ml/mode-line)

(add-hook 'dired-mode-hook #'dired-omit-mode)

;; [[ Company-specific ]]
;; These files are only included if they exist (I'll have them on company machines).

(measure-time
 "Loading vm-arista"
 (condition-case nil
     (require 'vm-arista)
   (file-error nil)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %.3f seconds" (float-time (time-subtract (current-time) before-init-time)))
            (message "- emacs-init-time: %s" (emacs-init-time))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(a4-enable-default-bindings nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("ef4337d1a6504a8110e7b3b2f30e9b1df5cce78afa8a7a44afcc6ec7fce228e3" "c43813c439df1a3d5d373b7d91f628caffcb4ecdb908e562200a7b061e6e4cbc" "b1b5502bc64071b2e263ecfcb01bd1d784497c795c61c2cb6de4a3f459a91da9" "4c22e0a991f91a6a166699a8f267112934478db6592977c74926173f7f6c8071" "6072798c95eeda3719f455590248f2a641c440418cad564f0853d91ad7641944" "724ec1789ab57edf52040cf39280c0e09e2a8f0b0556695569e5ba3986fc183d" "6cd3c963db7aa40c9c0abf5caa923c4205a885fe583f4fd1c33e722b3535a763" "0b08daef5c9b853c1bf82a0797bcd8d4d333be2dbe7aba402064a9653196991d" "d5b6892dabfa54f659918326e459dcc3f4851724759063c1ff2c3e43c734b6cb" "2d405365e4edaf423465d961b8bcc09f4d280af05ab0319924f7017c1fcf136d" "3a65dffab04340599fb2daf6e8db5349f65b9c0403a3b98b5927442ca97c16b1" "4ce77ae7163893c4dd8629d00aa7a26013b680e4be59021e2d2a80544ab34939" "998c811d828dbc02eff645e633dfcc90e02ebdad9558a457e622be1335de211b" "19aca151c4a38aefee68109e6701d2555fadd987cbf12e7d90b5af4e66d89548" "6538d61c331f93f2089e5e53141798ca954e2dd4eb43e773b288c0d15ffc8d6c" "fe08a51edfb96058e164f2c013a2b17a6114aaf6c6dc7fc6ce28df4736a46c83" "6a18e904da7918d42a6d9bc1d6936b13fc48763e3dc87e0df87a3ed893b6b7b8" "fa09c11029549fc9ae9088772034aae80ec3d91c25b09e58f23a2ae30435406d" "c6a3b79fbe9462a6f057d941a959e71d3945158424fc05ec46204d58e2d182a0" "821c37a78c8ddf7d0e70f0a7ca44d96255da54e613aa82ff861fe5942d3f1efc" "835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f" default))
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhv")
 '(eat-enable-auto-line-mode t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(erc-fill-column 85)
 '(fill-column 85)
 '(grep-command "rg -nS --no-heading ''")
 '(grep-command-position 22)
 '(grep-use-null-device nil)
 '(indent-tabs-mode nil)
 '(org-agenda-window-setup 'current-window)
 '(org-log-into-drawer t)
 '(package-selected-packages
   '(highlight-escape-sequences which-key eglot yaml-mode markdown-mode vlf font-lock-studio cape corfu-terminal corfu clipetty rg acme-theme ace-window git-gutter tm42-buffer-groups expand-region org-roam avy move-text multiple-cursors zig-mode orderless consult marginalia vertico vterm xcscope magit))
 '(pylint-command "a git pylint")
 '(ring-bell-function 'ignore)
 '(scroll-conservatively 1000)
 '(scroll-margin 3)
 '(scroll-preserve-screen-position 1)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#FAFBFC" :foreground "#2A3C4B" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 200 :width normal :foundry "nil" :family "Berkeley Mono"))))
 '(font-lock-comment-face ((t (:foreground "#5A7387" :slant normal))))
 '(mode-line-active ((t (:inherit mode-line :box (:line-width (1 . 7) :color "#E5F0F7")))))
 '(mode-line-inactive ((t (:background "#EDEDED" :foreground "#8AA2B0" :box (:line-width (1 . 7) :color "#EDEDED")))))
 '(tab-bar ((t (:inherit mode-line-inactive :box nil)))))
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

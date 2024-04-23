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

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

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

;; Saves window layouts. To return to previous window layout,
;; do C-c LEFT_ARROW.
(winner-mode 1)

(global-hl-line-mode 1)
(setq global-hl-line-sticky-flag t)

;;; [[ Movement ]]

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
; (global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
; (global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

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
  (global-set-key (kbd "C-:") 'avy-goto-char-timer))

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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/Users/vmalladi/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a
  ;; more informative completion interface
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

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

;; Debug

(defcustom tm42-buffer-groups-debug nil
  "Enable debug messages for the package."
  :type 'boolean
  :group 'tm42-buffer-groups)

(defun tm42-buffer-groups-debug-message (format-string &rest args)
  "Print debug message if debugging is enabled."
  (when tm42-buffer-groups-debug
    (apply 'message (concat "[tm42-buffer-groups] " format-string) args)))

;; State

(defvar tm42-group-to-buffers-map (make-hash-table :test 'equal)
  "Hash table whose keys are buffer group names and values are lists
of buffers belonging to a buffer group.")
(defun tm42-group-to-buffers-map-init ()
  (clrhash tm42-group-to-buffers-map)
  (puthash "Other" (mapcar 'buffer-name (buffer-list)) tm42-group-to-buffers-map)
  tm42-group-to-buffers-map)

(defvar tm42-buffer-to-group-map (make-hash-table :test 'equal)
  "Hash table whose keys are buffer names and whose values are the group
that buffer belongs to.")
(defun tm42-buffer-to-group-map-init ()
  (clrhash tm42-buffer-to-group-map)
  (mapcar '(lambda (buf)
             (puthash (buffer-name buf) "Other" tm42-buffer-to-group-map))
          (buffer-list))
  tm42-buffer-to-group-map)

(defun tm42-get-group-for-buffer (buf-name)
  (gethash buf-name tm42-buffer-to-group-map))
(defun tm42-get-buffers-in-group (group-name)
  (gethash group-name tm42-group-to-buffers-map))

(defun tm42-init-buffer-groups ()
  (tm42-group-to-buffers-map-init)
  (tm42-buffer-to-group-map-init))
(defun tm42-clear-buffer-groups ()
  (clrhash tm42-group-to-buffers-map)
  (clrhash tm42-buffer-to-group-map))
(defun tm42-set-buffer-group (group-name group-bufs)
  "Assign a group of buffers, group-bufs, to the group group-name.
This overrides any existing assignments to and from this group."
  (let ((prev-group-bufs (gethash group-name tm42-group-to-buffers-map '())))
    (dolist (prev-grouped-buf prev-group-bufs)
      (remhash prev-grouped-buf tm42-buffer-to-group-map))
    (puthash group-name group-bufs tm42-group-to-buffers-map)
    (mapcar #'(lambda (group-buf)
                (puthash group-buf group-name tm42-buffer-to-group-map))
            group-bufs)))
(defun tm42-assign-buffer-to-group (group-name buf-name)
  "Does not check if buffer is already in group."
  (let ((prev-group-bufs (gethash group-name tm42-group-to-buffers-map '())))
    (puthash group-name
             (append prev-group-bufs (list buf-name))
             tm42-group-to-buffers-map)
    (puthash buf-name group-name tm42-buffer-to-group-map)))
(defun tm42-remove-buffer-from-buffer-groups (buf-name)
  (let* ((buf-group (tm42-get-group-for-buffer buf-name))
         (group-bufs (tm42-get-buffers-in-group buf-group)))
    (remhash buf-name tm42-buffer-to-group-map)
    (puthash buf-group
             (remove buf-name group-bufs)
             tm42-group-to-buffers-map)))
    

(tm42-init-buffer-groups)

;; Functionality

(defun tm42-refresh-buffer-groups ()
  (let ((buf-list (mapcar 'buffer-name (buffer-list)))
        (other-bufs (gethash "Other" tm42-group-to-buffers-map '())))
    (maphash #'(lambda (grouped-buf buf-group)
                (setq buf-list (remove grouped-buf buf-list)))
             tm42-buffer-to-group-map)
    (puthash "Other" (append other-bufs buf-list) tm42-group-to-buffers-map)
    (mapcar #'(lambda (buf)
               (puthash buf "Other" tm42-buffer-to-group-map))
            buf-list)))

(defun tm42-organize-buffers ()
  "Organize buffers into user-defined groups."
  (interactive)
  (let ((buf (get-buffer-create "*tm42-organize-buffers*")))
    (tm42-refresh-buffer-groups)
    (with-current-buffer buf
      (erase-buffer)
      (insert "---")
      (insert "\n")
      (maphash #'(lambda (group-name grouped-bufs)
                   (insert "\nGROUP: " group-name "\n")
                   (dolist (grouped-buf grouped-bufs)
                     (insert grouped-buf "\n")))
               tm42-group-to-buffers-map)
      (insert "\n\n")
      (insert "---")
      (fundamental-mode)
      (switch-to-buffer buf))))

(defun tm42-organize-buffers-update ()
  "Parse the *tm42-organize-buffers* buffer to update tm42-buffer-groups."
  (interactive)
  (let ((groups '())
        (current-group nil)
        (current-group-bufs '())
        (started nil))
    (with-current-buffer "*tm42-organize-buffers*"
      (goto-char (point-min))

      (while (and (not started) (not (eobp)))
        (setq started (looking-at "---"))
        (forward-line 1))
      
      (while (and (not (eobp)) (not (looking-at "---")))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))          
          (if (string-match-p "^[[:space:]]*$" line)
              (when current-group
                (tm42-set-buffer-group current-group current-group-bufs)
                (setq current-group nil)
                (setq current-group-bufs '()))
            (if (string-prefix-p "GROUP: " line)
                (setq current-group (substring line 7 nil))
              (setq current-group-bufs (append current-group-bufs (list line)))))
        (forward-line 1))))))

(defun tm42-forward-buffer (n)
  "Switch forward n buffers in the current group. Negative n means backwards."
  (let* ((group-name (gethash (buffer-name) tm42-buffer-to-group-map))
         (buffers-in-group (gethash group-name tm42-group-to-buffers-map))
         (current-buf-idx (cl-position (buffer-name) buffers-in-group :test 'string=)))
    (switch-to-buffer (tm42-cyclic-nth n current-buf-idx buffers-in-group))))

(defun tm42-next-buffer ()
  (interactive)
  (tm42-forward-buffer 1))

(defun tm42-previous-buffer ()
  (interactive)
  (tm42-forward-buffer -1))

;; Automatic buffer group assignment

(defvar tm42-curr-buffer-group "Other")

(defun tm42-new-buffer-hook (frame)
  (when (not (active-minibuffer-window))
    (let* ((win (frame-selected-window frame))
           (new-buf-name (buffer-name (window-buffer win)))
           (prev-buf-name (buffer-name (car (car (window-prev-buffers win)))))
           (prev-buf-group (tm42-get-group-for-buffer prev-buf-name)))
      (when (not (gethash new-buf-name tm42-buffer-to-group-map))
        (tm42-buffer-groups-debug-message
         "adding buffer %s to group %s of prev buffer %s"
         new-buf-name prev-buf-group prev-buf-name)
        (tm42-assign-buffer-to-group prev-buf-group new-buf-name)))))
(add-hook 'window-buffer-change-functions #'tm42-new-buffer-hook)

(defun tm42-kill-buffer-hook ()
  (tm42-remove-buffer-from-buffer-groups (buffer-name)))
(add-hook 'kill-buffer-hook #'tm42-kill-buffer-hook)

;; Config

(defvar tm42-buffer-group-keybindings-enabled nil)
(defun tm42-toggle-buffer-group-keybindings ()
  (interactive)
  (if tm42-buffer-group-keybindings-enabled
      (progn
        (global-set-key (kbd "C-x <right>") 'next-buffer)
        (global-set-key (kbd "C-x <left>") 'previous-buffer))
    (progn
      (global-set-key (kbd "C-x <right>") 'tm42-next-buffer)
      (global-set-key (kbd "C-x <left>") 'tm42-previous-buffer))))
    
;; [[ Mode line ]]

(defface vm-mode-line-normal-face
  '((t))
  "Used for normal text."
  :group 'vm-mode-line-group)
(defface vm-mode-line-bold-face
  '((t
     :inherit 'vm-mode-line-normal-face
     :weight bold))
  "Just a bold face."
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

(defun vm-mode-line ()
  (interactive)
  (setq-default mode-line-format
	        '((:propertize " "
                               face vm-mode-line-normal-face)
                  (:eval
                   (if (buffer-modified-p)
                       (propertize "\\/"
                                   'face
                                   'vm-mode-line-unsaved-face)
                     (propertize "/\\"
                                 'face
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
		  (:propertize "    "
			       face vm-mode-line-normal-face)
		  (:propertize "%b" face vm-mode-line-bold-face)
		  )
	        ))

(defun vm-default-mode-line ()
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

(vm-mode-line)

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
   '(expand-region org-roam avy move-text multiple-cursors zig-mode orderless consult marginalia vertico vterm xcscope magit))
 '(scroll-preserve-screen-position 1)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal)))))

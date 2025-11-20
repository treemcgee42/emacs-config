;;; tm42-claude-config.el --- Claude Code and Monet configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: convenience, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; This module provides configuration for Claude Code and Monet integration
;; in Emacs, using elpaca as the package manager and eat as the terminal backend.
;;
;; Usage:
;;   (require 'tm42-claude-config)
;;   (tm42-claude-setup)

;;; Code:

(require 'elpaca)

;;; Customization Variables

(defgroup tm42-claude nil
  "Configuration for Claude Code and Monet."
  :group 'tools
  :prefix "tm42-claude-")

(defcustom tm42-claude-terminal-backend 'eat
  "Terminal backend to use for Claude Code."
  :type '(choice (const :tag "Eat" eat)
                 (const :tag "Vterm" vterm))
  :group 'tm42-claude)

(defcustom tm42-claude-prefix-key "C-c c"
  "Prefix key for Claude Code commands."
  :type 'string
  :group 'tm42-claude)

(defcustom tm42-claude-monet-prefix-key "C-c m"
  "Prefix key for Monet commands."
  :type 'string
  :group 'tm42-claude)

(defcustom tm42-claude-enable-auto-revert t
  "Enable global auto-revert mode for Claude file changes."
  :type 'boolean
  :group 'tm42-claude)

(defcustom tm42-claude-scrollback-size 500000
  "Scrollback size for eat terminal in Claude buffers."
  :type 'integer
  :group 'tm42-claude)

;;; Setup Functions

(defun tm42-claude--setup-monet ()
  "Configure Monet after it's loaded."
  (require 'monet)

  ;; Enable Monet mode globally
  (monet-mode 1)

  ;; Set prefix key
  (setq monet-prefix-key tm42-claude-monet-prefix-key)

  (setq monet-diff-tool 'monet-ediff-tool)
  (setq monet-diff-cleanup-tool 'monet-ediff-cleanup-tool)

  ;; Optional: Customize diff tool (uncomment to use ediff instead of simple diff)
  ;; (setq monet-diff-tool #'monet-ediff-tool)
  ;; (setq monet-diff-cleanup-tool #'monet-ediff-cleanup-tool)

  (message "Monet configured successfully"))

(defun tm42-claude--setup-claude-code ()
  "Configure Claude Code after it's loaded."
  (require 'claude-code)

  ;; Set terminal backend
  (setq claude-code-terminal-backend tm42-claude-terminal-backend)

  ;; Terminal configuration
  (setq claude-code-term-name "xterm-256color")

  ;; Keybinding style for newlines in Claude buffer
  ;; Options: 'newline-on-shift-return, 'newline-on-alt-return,
  ;;          'shift-return-to-send, 'super-return-to-send
  (setq claude-code-newline-keybinding-style 'newline-on-shift-return)

  ;; Notifications
  (setq claude-code-enable-notifications t)

  ;; Buffer size threshold for confirmation
  (setq claude-code-large-buffer-threshold 100000)

  ;; Confirmation for killing instances
  (setq claude-code-confirm-kill t)

  ;; Window resize optimization
  (setq claude-code-optimize-window-resize t)

  ;; Auto-select Claude buffer when toggling
  (setq claude-code-toggle-auto-select nil)

  ;; Integrate Monet with Claude Code
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)

  ;; Enable claude-code-mode globally
  (claude-code-mode 1)

  ;; Set up key bindings
  (global-set-key (kbd tm42-claude-prefix-key) claude-code-command-map)

  ;; Optional: Set up repeat map for mode cycling
  ;; This allows you to press "M" repeatedly to cycle through modes
  ;; after the initial C-c c M invocation
  (defvar-keymap tm42-claude-code-repeat-map
    :repeat t
    "M" #'claude-code-cycle-mode)

  ;; Hooks and customization
  (add-hook 'claude-code-start-hook
            (lambda ()
              ;; Reduce eat latency for less flickering (optional)
              ;; (setq-local eat-minimum-latency 0.033
              ;;             eat-maximum-latency 0.1)

              ;; Reduce line spacing to fix vertical bar gaps (optional)
              ;; (setq-local line-spacing 0.1)

              ;; Increase scrollback for long conversations
              (setq-local eat-term-scrollback-size tm42-claude-scrollback-size)))

  ;; Optional: Custom notification function
  ;; Uncomment and customize as needed
  ;; (defun tm42-claude-notify (title message)
  ;;   "Display a custom notification for Claude Code."
  ;;   (message "[%s] %s" title message))
  ;; (setq claude-code-notification-function #'tm42-claude-notify)

  (message "Claude Code configured successfully"))

(defun tm42-claude--setup-fonts ()
  "Configure fonts for proper Unicode rendering.
This is optional and commented out by default."
  ;; Ensure proper Unicode rendering for Claude's special characters
  ;; Uncomment and adjust font names based on your preferences and system fonts

  ;; (setq use-default-font-for-symbols nil)
  ;;
  ;; ;; Add fallback fonts for symbols (least preferred first)
  ;; ;; Adjust these based on your system (macOS, Linux, Windows)
  ;; (set-fontset-font t 'symbol "STIX Two Math" nil 'prepend)  ; macOS
  ;; (set-fontset-font t 'symbol "Menlo" nil 'prepend)          ; macOS
  ;; ;; (set-fontset-font t 'symbol "DejaVu Sans Mono" nil 'prepend)  ; Linux
  ;;
  ;; ;; Add your preferred default font last
  ;; (set-fontset-font t 'symbol "Your Default Font" nil 'prepend)
  ;;
  ;; ;; Alternative: Use JuliaMono for excellent Unicode support
  ;; ;; (set-fontset-font t 'unicode (font-spec :family "JuliaMono"))
  )

(defun tm42-claude--setup-display ()
  "Configure display buffer settings.
This is optional and commented out by default."
  ;; Optional: Configure how Claude buffers are displayed
  ;; Uncomment and customize as needed

  ;; Example: Display Claude in a right side window
  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*claude"
  ;;                (display-buffer-in-side-window)
  ;;                (side . right)
  ;;                (window-width . 90)))
  )

(defun tm42-claude--setup-auto-revert ()
  "Configure auto-revert to sync buffers with Claude's file changes."
  (when tm42-claude-enable-auto-revert
    ;; Enable global auto-revert to sync buffers with Claude's file changes
    (global-auto-revert-mode 1)

    ;; Optional: Disable file notification and use polling if auto-revert is unreliable
    ;; (setq auto-revert-use-notify nil)
    ))

;;;###autoload
(defun tm42-claude-setup ()
  "Initialize Claude Code and Monet configuration.
This function installs and configures all necessary packages."
  (interactive)

  ;; Install/upgrade transient (required by claude-code.el)
  ;; Explicitly install from source to override built-in
  (elpaca transient
    (require 'transient))

  ;; Install inheritenv (required by claude-code.el)
  (elpaca inheritenv
    (require 'inheritenv))

  ;; Install eat terminal (backend for claude-code.el)
  (elpaca eat
    (require 'eat))

  ;; Install and configure Monet
  (elpaca (monet :host github :repo "stevemolitor/monet")
    (tm42-claude--setup-monet))

  ;; Install and configure Claude Code
  ;; This needs to wait for Monet to be loaded first
  (elpaca (claude-code :host github :repo "stevemolitor/claude-code.el")
    (tm42-claude--setup-claude-code))

  ;; Wait for all packages to be installed
  (elpaca-wait)

  ;; Set up auto-revert
  (tm42-claude--setup-auto-revert)

  ;; Optional setups (commented out by default)
  ;; (tm42-claude--setup-fonts)
  ;; (tm42-claude--setup-display)

  (message "Claude Code and Monet configuration loaded successfully"))

;;;###autoload
(defun tm42-claude-reload ()
  "Reload Claude Code and Monet configuration.
Useful after making changes to customization variables."
  (interactive)
  (when (featurep 'claude-code)
    (tm42-claude--setup-claude-code))
  (when (featurep 'monet)
    (tm42-claude--setup-monet))
  (tm42-claude--setup-auto-revert)
  (message "Claude Code and Monet configuration reloaded"))

;;; Provide

(provide 'tm42-claude-config)

;;; tm42-claude-config.el ends here

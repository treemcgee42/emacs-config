;;; tm42-eshell-header.el --- Show last Eshell command in header-line -*- lexical-binding: t; -*-

;; Author: Varun Malladi
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: eshell, convenience, ui
;; URL: https://github.com/tm42/tm42-eshell-header

;;; Commentary:
;;
;; Displays the last executed Eshell command in the header line.
;; The command text is captured exactly as it’s sent to Eshell,
;; whether it comes from the current line or an active region.
;;
;; Example:
;;   ───────────────────────────────────────────────
;;    last: git status
;;   ───────────────────────────────────────────────
;;
;; Enable it automatically in all Eshell buffers:
;;
;;   (add-hook 'eshell-mode-hook #'tm42-eshell-header-mode)
;;
;;; Code:

(defgroup tm42-eshell-header nil
  "Display the last Eshell command in the header line."
  :group 'eshell
  :prefix "tm42-eshell-header-")

(defcustom tm42-eshell-header-prefix "last:"
  "Prefix text displayed before the last command."
  :type 'string
  :group 'tm42-eshell-header)

(defcustom tm42-eshell-header-prefix-face '(:foreground "gray50")
  "Face applied to the header prefix."
  :type 'face
  :group 'tm42-eshell-header)

(defcustom tm42-eshell-header-command-face '(:foreground "cyan")
  "Face applied to the command text."
  :type 'face
  :group 'tm42-eshell-header)

(defvar-local tm42-eshell-header--text ""
  "Holds the last Eshell command for display in the header line.")

(defun tm42-eshell-header--capture ()
  "Capture the exact text Eshell is about to send."
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (line-end-position)))
                     (goto-char end)
                     (buffer-substring-no-properties (eshell-bol) end))))))
    (setq tm42-eshell-header--text (string-trim text))
    (force-mode-line-update)))

(defun tm42-eshell-header--advice (&rest _)
  "Advice to capture the command just before Eshell sends it."
  (when (derived-mode-p 'eshell-mode)
    (tm42-eshell-header--capture)))

(defun tm42-eshell-header--format ()
  "Return the formatted header-line text."
  (when (and (derived-mode-p 'eshell-mode)
             (stringp tm42-eshell-header--text)
             (not (string-empty-p tm42-eshell-header--text)))
    (format " %s %s "
            (propertize tm42-eshell-header-prefix
                        'face tm42-eshell-header-prefix-face)
            (propertize tm42-eshell-header--text
                        'face tm42-eshell-header-command-face))))

(defun tm42-eshell-header--setup ()
  "Activate header-line updates for this Eshell buffer."
  (setq header-line-format '((:eval (tm42-eshell-header--format))))
  (advice-add 'eshell-send-input :before #'tm42-eshell-header--advice))

(defun tm42-eshell-header--teardown ()
  "Remove header-line updates from this Eshell buffer."
  (kill-local-variable 'header-line-format)
  (advice-remove 'eshell-send-input #'tm42-eshell-header--advice))

;;;###autoload
(define-minor-mode tm42-eshell-header-mode
  "Minor mode to show the last Eshell command in the header line."
  :lighter ""
  (if tm42-eshell-header-mode
      (tm42-eshell-header--setup)
    (tm42-eshell-header--teardown)))

;;;###autoload
(defun tm42-eshell-header-enable ()
  "Enable `tm42-eshell-header-mode' in all Eshell buffers."
  (add-hook 'eshell-mode-hook #'tm42-eshell-header-mode))

(provide 'tm42-eshell-header)
;;; tm42-eshell-header.el ends here

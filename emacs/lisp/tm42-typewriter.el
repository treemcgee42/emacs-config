;;; tm42-typewriter.el --- Gentle typewriter mode with peek/respect  -*- lexical-binding: t; -*-

;; Minimal, no deps. M-x tm42-typewriter-mode to toggle.

(defgroup tm42-typewriter nil
  "Keep point at a preferred screen position without fighting peeks."
  :group 'convenience)

(defcustom tm42-typewriter-position 0.5
  "Preferred vertical position for point as a fraction of window body height.
0.0 = top line, 0.5 = center, 0.66 ≈ bottom third."
  :type 'number)

(defcustom tm42-typewriter-peek-seconds 1.0
  "How long to pause after manual scroll/peek before re-centering resumes."
  :type 'number)

(defcustom tm42-typewriter-only-on-line-change t
  "If non-nil, only recenter when the current line number changes.
Reduces jitter while editing within the same line."
  :type 'boolean)

(defvar-local tm42-typewriter--last-line nil)
(defvar-local tm42-typewriter--peek-until 0.0)

(defun tm42-typewriter--body-lines ()
  "Return window body height in *screen* lines."
  (max 1 (1- (window-body-height nil 'count-screen-lines))))

(defun tm42-typewriter--current-screen-line ()
  "Return the 0-based screen line index of point within the window."
  (let ((ws (window-start)))
    (min (tm42-typewriter--body-lines)
         (max 0 (count-screen-lines ws (point) t)))))

(defun tm42-typewriter--now () (float-time (current-time)))

(defun tm42-typewriter--pause-peek ()
  "Pause enforcing recentering briefly (used after manual scroll)."
  (setq tm42-typewriter--peek-until (+ (tm42-typewriter--now)
                                       tm42-typewriter-peek-seconds)))

(defun tm42-typewriter--interactive-p ()
  "Return non-nil if the current command is user-invoked (not internal)."
  (and this-command (not (eq this-command 'tm42-typewriter--enforce))))

(defun tm42-typewriter--learn-from-current ()
  "Set target to the *current* on-screen position of point."
  (interactive)
  (let* ((h (max 1 (tm42-typewriter--body-lines)))
         (lin (float (tm42-typewriter--current-screen-line))))
    (setq tm42-typewriter-position (min 1.0 (max 0.0 (/ lin h))))))

(defun tm42-typewriter--recenter ()
  "Recenter point to the configured `tm42-typewriter-position`."
  (let* ((h (tm42-typewriter--body-lines))
         ;; recenter’s arg is 1-based line number from top.
         (line (1+ (truncate (* tm42-typewriter-position h)))))
    (recenter line)))

(defun tm42-typewriter--should-recenter ()
  "Predicate: should we recenter now?"
  (and (not (active-minibuffer-window))
       (<= (tm42-typewriter--now) (+ 1e9)) ;; dummy to force numberp
       (> (tm42-typewriter--now) tm42-typewriter--peek-until)
       (or (not tm42-typewriter-only-on-line-change)
           (let ((ln (line-number-at-pos)))
             (prog1 (not (eq ln tm42-typewriter--last-line))
               (setq tm42-typewriter--last-line ln))))))

(defun tm42-typewriter--enforce ()
  "Post-command hook body."
  (when (and tm42-typewriter-mode
             (tm42-typewriter--should-recenter))
    (tm42-typewriter--recenter)))

;; --- Integration points ------------------------------------------------

;; 1) Respect manual recenter (C-l / `recenter-top-bottom`): learn target.
(defun tm42-typewriter--advice-recenter (&rest _args)
  "When user hits C-l, adopt the new on-screen position."
  (when (and tm42-typewriter-mode (tm42-typewriter--interactive-p))
    (tm42-typewriter--learn-from-current)
    ;; Don’t immediately fight: allow a short peek window.
    (tm42-typewriter--pause-peek)))

;; 2) Respect peeking via scroll commands and mouse wheel.
(dolist (cmd '(scroll-up-command scroll-down-command
               pixel-scroll-up pixel-scroll-down
               mwheel-scroll))
  (advice-add cmd :after (lambda (&rest _)
                           (when tm42-typewriter-mode
                             (tm42-typewriter--pause-peek)))))

;; 3) Adopt position after recenter calls.
(advice-add 'recenter-top-bottom :after #'tm42-typewriter--advice-recenter)
(advice-add 'recenter :after #'tm42-typewriter--advice-recenter)

;; --- Minor mode --------------------------------------------------------

;;;###autoload
(define-minor-mode tm42-typewriter-mode
  "Keep point at a preferred vertical screen position.
Learns from manual C-l and pauses after peeks so it doesn't fight you."
  :init-value nil
  :lighter " tw+"
  (if tm42-typewriter-mode
      (progn
        (setq tm42-typewriter--last-line (line-number-at-pos)
              tm42-typewriter--peek-until 0.0)
        (add-hook 'post-command-hook #'tm42-typewriter--enforce nil t))
    (remove-hook 'post-command-hook #'tm42-typewriter--enforce t)))

;; --- Convenience commands ---------------------------------------------

(defun tm42-typewriter-set-position (fraction)
  "Set the target vertical FRACTION (0.0 top ... 1.0 bottom) interactively."
  (interactive "nTarget fraction (0=top, .5=center, .66=bottom third): ")
  (setq tm42-typewriter-position (min 1.0 (max 0.0 fraction)))
  (when tm42-typewriter-mode
    (tm42-typewriter--recenter)))

(defun tm42-typewriter-center () (interactive) (tm42-typewriter-set-position 0.5))
(defun tm42-typewriter-bottom-third () (interactive) (tm42-typewriter-set-position 0.66))
(defun tm42-typewriter-top-third () (interactive) (tm42-typewriter-set-position 0.33))

(provide 'tm42-typewriter)
;;; tm42-typewriter.el ends here

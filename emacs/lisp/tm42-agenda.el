;;; tm42-agenda.el --- TODO                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026

(defcustom tm42/org-agenda-dir "~/Desktop/org-agenda"
  "Directory where the org (agenda) files are.")

(setf org-agenda-files (list tm42/org-agenda-dir))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WIP(p)" "|" "WAIT(w)" "DONE(d)")))

(setq org-todo-keyword-faces
      '(("TODO" . "orange")
        ("NEXT" . "pink")
        ("WIP"  . "yellow")
        ("WAIT" . "green")
        ("DONE" . "green")))

(setq org-capture-templates
      `(("t" "Task" entry
         (file ,(file-name-concat tm42/org-agenda-dir "inbox.org"))
         "* NEXT %?\n%U")
        ("b" "Bug" entry
         (file+headline ,(file-name-concat tm42/org-agenda-dir "inbox.org") "Bugs")
         "* NEXT BUG%^{Bug ID} -- %^{Title}  :%^{Triage type|reltriage|triage}:
:PROPERTIES:
:BUG:      %\\1
:END:
%U\n")))

(setq org-agenda-custom-commands
      '(("w" "Work dashboard"
         ((agenda "")
          (tags "daily"
                ((org-agenda-overriding-header "Tasks for today")))
          (tags "reltriage"
                ((org-agenda-overriding-header "Release triage")))
          (tags-todo "-daily-reltriage+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Next actions")))
          (tags-todo "+LEVEL=1+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Projects")))))))

(setq org-refile-targets
      `((,(file-name-concat tm42/org-agenda-dir "filed-tasks.org")
         :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun tm42/agenda/toggle-daily-task ()
  "Toggle the 'daily' tag on the current Org heading."
  (interactive)
  (let ((tag "daily"))
    ;; If called from agenda, jump to the real Org heading
    (org-with-point-at (or (org-get-at-bol 'org-hd-marker) (point))
      (org-back-to-heading t)
      (let ((tags (org-get-tags nil nil))) ;; explicit tags only
        (if (member tag tags)
            (setq tags (delete tag tags))  ;; remove
          (push tag tags))                 ;; add
        (org-set-tags (delete-dups tags))))))

(defun tm42/agenda/clear-daily-tasks ()
  "Remove the 'daily' tag from all agenda files."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-back-to-heading t)
     (let* ((tag "daily")
            (tags (org-get-tags nil nil)))
       (when (member tag tags)
         (org-set-tags (delete tag tags)))))
   "+daily"             ;; match headings with the tag
   'agenda))            ;; search all agenda files

(defun tm42/agenda/archive-done-tasks-in-file ()
  "Archive all DONE tasks in the current Org file."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun tm42/agenda/archive-all-done-tasks ()
  "Archive all DONE tasks in all `org-agenda-files`."
  (interactive)
  (dolist (file (org-agenda-files))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (tm42/org-archive-done-tasks-in-file))
      (save-buffer))))

(provide 'tm42-agenda)

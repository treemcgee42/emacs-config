;;; tm42-runner.el --- Task runner  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'vtable)

(defvar tm42/runner/tasks nil
  "List of tasks (NAME . CMD).")

(defvar-local tm42/runner/last-task nil
  "Last-run task name (buffer-local).")

(defvar-local tm42/runner/--table nil
  "The vtable object in this buffer (buffer-local).")

(defconst tm42/runner--buffer "*tm42-runner*")

(defun tm42/runner--sorted-tasks ()
  (sort (copy-sequence tm42/runner/tasks)
        (lambda (a b) (string< (car a) (car b)))))

(defun tm42/runner--cmd (name)
  (cdr (assoc name tm42/runner/tasks)))

(defun tm42/runner--upsert (name cmd)
  (let ((cell (assoc name tm42/runner/tasks)))
    (if cell
        (setcdr cell cmd)
      (push (cons name cmd) tm42/runner/tasks))))

(defun tm42/runner--delete (name)
  (setq tm42/runner/tasks
        (cl-remove name tm42/runner/tasks :key #'car :test #'string=)))

(defun tm42/runner--read-task ()
  "Read (NAME . CMD) from minibuffer."
  (let* ((name (string-trim (completing-read "Task name: "
                                            (mapcar #'car tm42/runner/tasks)
                                            nil nil)))
         (cmd  (string-trim (read-shell-command "Compile command: "))))
    (when (string-empty-p name) (user-error "Task name can't be empty"))
    (when (string-empty-p cmd)  (user-error "Command can't be empty"))
    (cons name cmd)))

(defun tm42/runner--current-object ()
  "Return the current vtable object (a (NAME . CMD) cons), or error."
  (or (vtable-current-object)
      (user-error "Not on a task row")))

(defun tm42/runner--current-name ()
  (car (tm42/runner--current-object)))

(defun tm42/runner--revert (&optional preserve-object)
  "Re-generate the vtable, preserving PRESERVE-OBJECT selection if possible."
  (when tm42/runner/--table
    (let ((obj preserve-object))
      (save-excursion
        (vtable-goto-table tm42/runner/--table)
        (vtable-revert-command))
      (when obj
        (ignore-errors
          (vtable-goto-object obj))))))

(defun tm42/runner-run (object)
  "Run OBJECT (a (NAME . CMD) task)."
  (interactive (list (tm42/runner--current-object)))
  (let* ((name (car object))
         (cmd  (cdr object)))
    (unless (and cmd (not (string-empty-p cmd)))
      (user-error "No command for %S" name))
    (setq tm42/runner/last-task name)
    ;; Refresh so the “Last” column updates, but keep selection on this row.
    (tm42/runner--revert object)
    (compile cmd)))

(defun tm42/runner-rerun ()
  (interactive)
  (unless tm42/runner/last-task
    (user-error "No last task yet"))
  (let ((cmd (tm42/runner--cmd tm42/runner/last-task)))
    (unless cmd (user-error "Last task not found in task list"))
    ;; Refresh first so UI is accurate even if command errors etc.
    (tm42/runner--revert nil)
    (compile cmd)))

(defun tm42/runner-add ()
  (interactive)
  (pcase-let ((`(,name . ,cmd) (tm42/runner--read-task)))
    (tm42/runner--upsert name cmd)
    (tm42/runner--revert nil)
    ;; try to jump to the newly-added row
    (let ((obj (assoc name (tm42/runner--sorted-tasks))))
      (when obj (ignore-errors (vtable-goto-object obj))))
    (message "Added %s" name)))

(defun tm42/runner-edit (object)
  (interactive (list (tm42/runner--current-object)))
  (let* ((name (car object))
         (old  (cdr object))
         (new  (string-trim (read-shell-command (format "New cmd for %s: " name) old))))
    (when (string-empty-p new) (user-error "Command can't be empty"))
    (tm42/runner--upsert name new)
    (tm42/runner--revert (assoc name (tm42/runner--sorted-tasks)))
    (message "Updated %s" name)))

(defun tm42/runner-edit (object)
  "Edit OBJECT (a (NAME . CMD) task), allowing rename."
  (interactive (list (tm42/runner--current-object)))
  (let* ((old-name (car object))
         (old-cmd  (cdr object))
         (new-name (string-trim
                    (read-string
                     (format "Rename task (%s): " old-name)
                     old-name)))
         (new-cmd  (string-trim
                    (read-shell-command
                     (format "Command for %s: " new-name)
                     old-cmd))))

    (when (string-empty-p new-name)
      (user-error "Task name can't be empty"))
    (when (string-empty-p new-cmd)
      (user-error "Command can't be empty"))

    ;; If renaming to an existing different task, confirm overwrite.
    (when (and (not (equal new-name old-name))
               (assoc new-name tm42/runner/tasks))
      (unless (yes-or-no-p
               (format "Task '%s' exists. Overwrite? " new-name))
        (user-error "Aborted")))

    ;; Remove old entry
    (setq tm42/runner/tasks
          (cl-remove old-name tm42/runner/tasks
                     :key #'car :test #'string=))

    ;; Insert updated entry
    (push (cons new-name new-cmd) tm42/runner/tasks)

    ;; Update last-task if needed
    (when (equal tm42/runner/last-task old-name)
      (setq tm42/runner/last-task new-name))

    ;; Refresh and move to updated row
    (tm42/runner--revert nil)
    (let ((new-obj (assoc new-name (tm42/runner--sorted-tasks))))
      (when new-obj
        (ignore-errors (vtable-goto-object new-obj))))

    (message "Updated %s → %s" old-name new-name)))

(defun tm42/runner-delete (object)
  (interactive (list (tm42/runner--current-object)))
  (let ((name (car object)))
    (when (yes-or-no-p (format "Delete %s? " name))
      (tm42/runner--delete name)
      (when (equal tm42/runner/last-task name)
        (setq tm42/runner/last-task nil))
      (tm42/runner--revert nil)
      (message "Deleted %s" name))))

(defun tm42/runner-refresh (&optional _arg)
  (interactive)
  (tm42/runner--revert (ignore-errors (tm42/runner--current-object))))

(defvar-local tm42/runner/--last-echoed-task nil
  "Last task name we echoed the command for (buffer-local).")

(defun tm42/runner--echo-command ()
  "Echo command for current task when selection changes."
  (when (and (eq major-mode 'tm42-runner-mode)
             tm42/runner/--table)
    (let ((obj (ignore-errors (vtable-current-object))))
      (when obj
        (let* ((name (car obj))
               (cmd  (cdr obj)))
          (unless (equal name tm42/runner/--last-echoed-task)
            (setq tm42/runner/--last-echoed-task name)
            ;; Keep it short-ish but not truncated by default;
            ;; message will truncate visually if super long.
            (message "%s: %s" name cmd)))))))

(defvar tm42-runner-mode-map
  (let ((m (make-sparse-keymap)))
    ;; vtable provides navigation + sort + column resizing, etc.
    ;; We layer our commands on top.
    (define-key m (kbd "i") #'tm42/runner-add)
    (define-key m (kbd "e") #'tm42/runner-edit)
    (define-key m (kbd "d") #'tm42/runner-delete)
    (define-key m (kbd "r") #'tm42/runner-run)
    (define-key m (kbd "RET") #'tm42/runner-run)
    (define-key m (kbd "R") #'tm42/runner-rerun)
    (define-key m (kbd "g") #'tm42/runner-refresh) ;; also works as vtable refresh feel
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `tm42-runner-mode`.")

(define-derived-mode tm42-runner-mode special-mode "tm42-runner"
  "tm42 task runner (vtable)."
  (hl-line-mode 1))

(defun tm42/runner--make-table ()
  "Insert header + vtable at point, set `tm42/runner/--table`."
  (let ((keymap (copy-keymap tm42-runner-mode-map)))
    (setq tm42/runner/--table
          (make-vtable
           :columns `((:name "Task" :primary ascend :width "100%"))
           :objects-function #'tm42/runner--sorted-tasks
           :getter (lambda (object column table)
                     (pcase (vtable-column table column)
                       ("Task" (car object))
                       ("Last" (equal (car object) tm42/runner/last-task))))
           :actions '("RET" tm42/runner-run
                      "r"   tm42/runner-run
                      "d"   tm42/runner-delete
                      "e"   tm42/runner-edit)
           :keymap keymap))))

;;;###autoload
(defun tm42-runner ()
  "Open tm42 task runner."
  (interactive)
  (let ((buf (get-buffer-create tm42/runner--buffer)))
    (pop-to-buffer buf)
    (tm42-runner-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq-local tm42/runner/tasks (or tm42/runner/tasks
                                       '(("Build" . "make -k")
                                         ("Tests" . "make test"))))
      (setq-local tm42/runner/last-task tm42/runner/last-task)
      ;; (insert "=== tm42 Runner ===\n")
      ;; (insert "i add   e edit   d delete   r/RET run   R rerun   g refresh   q quit\n")
      ;; (insert "Tip: `S` sorts by current column. `{`/`}` resize current column.\n\n")
      (tm42/runner--make-table)
      (add-hook 'post-command-hook #'tm42/runner--echo-command nil t))
    ;; land on first row
    (ignore-errors
      (vtable-goto-table tm42/runner/--table)
      (forward-line 1))))

(provide 'tm42-runner)

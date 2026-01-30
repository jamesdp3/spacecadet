;;; init.el --- Emacs configuration for spacecadet MCP server -*- lexical-binding: t; -*-

;; This configuration is loaded with `emacs -Q --load` to ensure complete
;; isolation from the user's personal Emacs configuration.

;; Disable package initialization (we only need built-in org-mode)
(setq package-enable-at-startup nil)

;; Load org-mode
(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'json)

;; Get the directory where this init.el lives
(defvar spacecadet-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Root directory of the spacecadet project.")

;; Org directory: use SPACECADET_ORG_DIR env var, or default to tasks/ subdir
(defvar spacecadet-org-dir
  (or (getenv "SPACECADET_ORG_DIR")
      (expand-file-name "tasks" spacecadet-root))
  "Directory containing org files for spacecadet.")

;; Set org-agenda-files to the org directory
(setq org-agenda-files (list spacecadet-org-dir))

;; Org-mode settings for consistent behavior
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-priority-highest ?A)
(setq org-priority-lowest ?D)
(setq org-priority-default ?C)

;; Enable logging of state changes
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; org-id: don't track globally (not useful in batch mode)
(setq org-id-track-globally nil)

;; Deadline/schedule warnings
(setq org-deadline-warning-days 7)

;; Agenda view settings
(setq org-agenda-span 'day)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-include-diary nil)

;; Custom agenda commands
(setq org-agenda-custom-commands
      '(("d" "Day agenda" agenda ""
         ((org-agenda-span 'day)))
        ("w" "Week agenda" agenda ""
         ((org-agenda-span 'week)))
        ("t" "All TODOs" alltodo "")
        ("p" "Priority A tasks" tags-todo "+PRIORITY=\"A\"")
        ("n" "Next actions" todo "NEXT")
        ("W" "Waiting tasks" todo "WAITING")))

;;; ============================================================
;;; HELPERS
;;; ============================================================

(defun spacecadet--env (name)
  "Get environment variable NAME, return nil if empty."
  (let ((val (getenv name)))
    (if (and val (not (string-empty-p val))) val nil)))

(defun spacecadet--find-task (heading callback)
  "Find task whose heading equals HEADING exactly, then call CALLBACK.
CALLBACK is called with point on the matching heading.
Returns t if found, nil otherwise."
  (let ((found nil))
    (dolist (file (org-agenda-files))
      (when (and (not found) (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (when (and (not found)
                        (string-equal heading
                                      (org-get-heading t t t t)))
               (setq found t)
               (funcall callback)))))))
    found))

(defun spacecadet--find-task-by-id (id callback)
  "Find task with org ID equal to ID, then call CALLBACK.
CALLBACK is called with point on the matching heading.
Returns t if found, nil otherwise."
  (let ((found nil))
    (dolist (file (org-agenda-files))
      (when (and (not found) (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (when (and (not found)
                        (string-equal id (or (org-entry-get nil "ID") "")))
               (setq found t)
               (funcall callback)))))))
    found))

(defun spacecadet--find-task-smart (callback)
  "Find a task using SC_ID (preferred) or SC_HEADING from environment.
Calls CALLBACK with point on the matching heading. Returns t if found."
  (let ((id (spacecadet--env "SC_ID"))
        (heading (spacecadet--env "SC_HEADING")))
    (cond
     (id (spacecadet--find-task-by-id id callback))
     (heading (spacecadet--find-task heading callback))
     (t (princ (json-encode (list (cons 'status "error")
                                  (cons 'message "SC_ID or SC_HEADING required"))))
        (kill-emacs 1)))))

(defun spacecadet--json-list (lst)
  "Encode LST as JSON, ensuring empty list outputs [] not null."
  (if lst
      (json-encode lst)
    "[]"))

;;; ============================================================
;;; READ OPERATIONS
;;; ============================================================

(defun spacecadet-agenda-to-stdout (key)
  "Run agenda command KEY and output to stdout."
  (let ((org-agenda-buffer-name "*Org Agenda*"))
    (org-agenda nil key)
    (with-current-buffer org-agenda-buffer-name
      (princ (buffer-string)))
    (kill-buffer org-agenda-buffer-name)))

(defun spacecadet-agenda-for-date (date-string)
  "Get agenda for DATE-STRING (format: YYYY-MM-DD)."
  (let* ((date (org-read-date nil nil date-string))
         (org-agenda-buffer-name "*Org Agenda*")
         (org-agenda-span 'day)
         (org-agenda-start-day date))
    (org-agenda-list)
    (with-current-buffer org-agenda-buffer-name
      (princ (buffer-string)))
    (kill-buffer org-agenda-buffer-name)))

(defun spacecadet-agenda-for-range (start-date end-date)
  "Get agenda from START-DATE to END-DATE (format: YYYY-MM-DD)."
  (let* ((start (org-read-date nil nil start-date))
         (end (org-read-date nil nil end-date))
         (days (1+ (- (org-time-string-to-absolute end)
                      (org-time-string-to-absolute start))))
         (org-agenda-buffer-name "*Org Agenda*")
         (org-agenda-span days)
         (org-agenda-start-day start))
    (org-agenda-list)
    (with-current-buffer org-agenda-buffer-name
      (princ (buffer-string)))
    (kill-buffer org-agenda-buffer-name)))

(defun spacecadet-search-todo (state)
  "Search for tasks in TODO STATE."
  (let ((org-agenda-buffer-name "*Org Agenda*"))
    (org-todo-list state)
    (with-current-buffer org-agenda-buffer-name
      (princ (buffer-string)))
    (kill-buffer org-agenda-buffer-name)))

(defun spacecadet-match-query (match)
  "Run org-mode MATCH query and output to stdout."
  (let ((org-agenda-buffer-name "*Org Agenda*"))
    (org-tags-view nil match)
    (with-current-buffer org-agenda-buffer-name
      (princ (buffer-string)))
    (kill-buffer org-agenda-buffer-name)))

(defun spacecadet-tasks-to-json ()
  "Output all tasks as JSON array."
  (let ((tasks '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (let* ((heading (org-get-heading t t t t))
                    (todo-state (org-get-todo-state))
                    (id (org-entry-get nil "ID"))
                    (priority (org-entry-get nil "PRIORITY"))
                    (deadline (org-entry-get nil "DEADLINE"))
                    (scheduled (org-entry-get nil "SCHEDULED"))
                    (tags (org-get-tags))
                    (effort (org-entry-get nil "Effort")))
               (when todo-state
                 (push (list (cons 'id id)
                             (cons 'heading heading)
                             (cons 'todo todo-state)
                             (cons 'priority priority)
                             (cons 'deadline deadline)
                             (cons 'scheduled scheduled)
                             (cons 'tags (if tags (vconcat tags) []))
                             (cons 'effort effort)
                             (cons 'file (buffer-file-name)))
                       tasks))))))))
    (princ (spacecadet--json-list (nreverse tasks)))))

(defun spacecadet-match-to-json (match)
  "Run org-mode MATCH query and return results as JSON array."
  (let ((tasks '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (let* ((heading (org-get-heading t t t t))
                    (todo-state (org-get-todo-state))
                    (id (org-entry-get nil "ID"))
                    (priority (org-entry-get nil "PRIORITY"))
                    (deadline (org-entry-get nil "DEADLINE"))
                    (scheduled (org-entry-get nil "SCHEDULED"))
                    (tags (org-get-tags)))
               (when todo-state
                 (push (list (cons 'id id)
                             (cons 'heading heading)
                             (cons 'todo todo-state)
                             (cons 'priority priority)
                             (cons 'deadline deadline)
                             (cons 'scheduled scheduled)
                             (cons 'tags (if tags (vconcat tags) [])))
                       tasks))))
           match))))
    (princ (spacecadet--json-list (nreverse tasks)))))

;;; ============================================================
;;; WRITE OPERATIONS
;;; ============================================================

(defun spacecadet-add-task-from-env ()
  "Add a new task. All parameters read from environment variables:
  SC_HEADING, SC_PRIORITY, SC_TAGS, SC_DEADLINE, SC_SCHEDULED, SC_STATE, SC_FILE."
  (let* ((heading (spacecadet--env "SC_HEADING"))
         (priority (spacecadet--env "SC_PRIORITY"))
         (tags (spacecadet--env "SC_TAGS"))
         (deadline (spacecadet--env "SC_DEADLINE"))
         (scheduled (spacecadet--env "SC_SCHEDULED"))
         (state (or (spacecadet--env "SC_STATE") "TODO"))
         (file-name (or (spacecadet--env "SC_FILE") "tasks.org"))
         (target-file (expand-file-name file-name spacecadet-org-dir))
         (priority-str (if priority (format " [#%s]" (upcase priority)) ""))
         (tags-list (when tags (split-string tags ",")))
         (tags-str (if tags-list
                       (format "  :%s:" (mapconcat #'string-trim tags-list ":"))
                     ""))
         (headline (format "* %s%s %s%s" state priority-str heading tags-str)))
    (unless heading
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "SC_HEADING not set"))))
      (kill-emacs 1))
    ;; Ensure file exists with header
    (unless (file-exists-p target-file)
      (with-temp-file target-file
        (insert "#+TITLE: Spacecadet Tasks\n#+STARTUP: overview\n\n")))
    ;; Append the task
    (with-current-buffer (find-file-noselect target-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert headline "\n")
      ;; Planning lines go together immediately after headline
      (when deadline
        (insert (format "   DEADLINE: <%s>" deadline))
        (when scheduled
          (insert (format " SCHEDULED: <%s>" scheduled)))
        (insert "\n"))
      (when (and scheduled (not deadline))
        (insert (format "   SCHEDULED: <%s>\n" scheduled)))
      ;; Go back to the heading we just inserted and assign an org-id
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (save-buffer)
        (kill-buffer (current-buffer))
        (princ (json-encode (list (cons 'status "ok")
                                  (cons 'id task-id)
                                  (cons 'heading heading)
                                  (cons 'todo state)
                                  (cons 'file target-file))))))))

(defun spacecadet-update-task-from-env ()
  "Update a task. Parameters from environment variables:
  SC_ID or SC_HEADING, SC_NEW_STATE, SC_NEW_PRIORITY, SC_NEW_DEADLINE."
  (let* ((new-state (spacecadet--env "SC_NEW_STATE"))
         (new-priority (spacecadet--env "SC_NEW_PRIORITY"))
         (new-deadline (spacecadet--env "SC_NEW_DEADLINE")))
    (let ((found (spacecadet--find-task-smart
                  (lambda ()
                    (when new-state (org-todo new-state))
                    (when new-priority
                      (org-priority (string-to-char (upcase new-priority))))
                    (when new-deadline (org-deadline nil new-deadline))
                    (save-buffer)))))
      (if found
          (princ (json-encode (list (cons 'status "ok")
                                    (cons 'updated t))))
        (princ (json-encode (list (cons 'status "error")
                                  (cons 'message "Task not found"))))))))

(defun spacecadet-delete-task-from-env ()
  "Delete a task. SC_ID or SC_HEADING from environment."
  (let ((found (spacecadet--find-task-smart
                (lambda ()
                  (org-cut-subtree)
                  (save-buffer)))))
    (if found
        (princ (json-encode (list (cons 'status "ok")
                                  (cons 'deleted t))))
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "Task not found")))))))

(defun spacecadet-get-task-from-env ()
  "Get task details. SC_ID or SC_HEADING from environment."
  (let ((found (spacecadet--find-task-smart
                (lambda ()
                  (let* ((h (org-get-heading t t t t))
                         (id (org-entry-get nil "ID"))
                         (todo-state (org-get-todo-state))
                         (priority (org-entry-get nil "PRIORITY"))
                         (deadline (org-entry-get nil "DEADLINE"))
                         (scheduled (org-entry-get nil "SCHEDULED"))
                         (tags (org-get-tags))
                         (body (org-get-entry)))
                    (princ (json-encode
                            (list (cons 'id id)
                                  (cons 'heading h)
                                  (cons 'todo todo-state)
                                  (cons 'priority priority)
                                  (cons 'deadline deadline)
                                  (cons 'scheduled scheduled)
                                  (cons 'tags (if tags (vconcat tags) []))
                                  (cons 'body (string-trim (or body "")))
                                  (cons 'file (buffer-file-name))))))))))
    (unless found
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "Task not found")))))))

;;; ============================================================
;;; CLOCKING OPERATIONS
;;; ============================================================

(defun spacecadet-clock-in-from-env ()
  "Clock in to task. SC_ID or SC_HEADING from environment."
  (require 'org-clock)
  (let ((found (spacecadet--find-task-smart
                (lambda ()
                  (org-clock-in)
                  (save-buffer)))))
    (if found
        (princ (json-encode (list (cons 'status "ok")
                                  (cons 'clocked-in t)
                                  (cons 'time (format-time-string "%Y-%m-%d %H:%M:%S")))))
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "Task not found")))))))

(defun spacecadet-clock-out ()
  "Clock out of any running clock by finding open CLOCK entries in org files.
Since each batch invocation is a fresh process, we find open CLOCK lines
directly rather than relying on org-clock state."
  (require 'org-clock)
  (let ((found nil)
        (now (format-time-string "%Y-%m-%d %a %H:%M")))
    (dolist (file (org-agenda-files))
      (when (and (not found) (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (goto-char (point-min))
          ;; Find open CLOCK entry (has start time but no end time)
          (while (and (not found)
                      (re-search-forward
                       "^[ \t]*CLOCK: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)\\][ \t]*$"
                       nil t))
            (setq found t)
            (let* ((start-time (match-string 1))
                   (start-seconds (float-time (org-time-string-to-time
                                               (format "[%s]" start-time))))
                   (end-seconds (float-time))
                   (duration-minutes (round (/ (- end-seconds start-seconds) 60)))
                   (hours (/ duration-minutes 60))
                   (mins (% duration-minutes 60)))
              (beginning-of-line)
              (delete-region (point) (line-end-position))
              (insert (format "   CLOCK: [%s]--[%s] => %d:%02d"
                              start-time now hours mins))
              (save-buffer)
              (princ (json-encode (list (cons 'status "ok")
                                        (cons 'clocked-out t)
                                        (cons 'duration (format "%d:%02d" hours mins))
                                        (cons 'time now)))))))))
    (unless found
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "No open clock entry found")))))))

(defun spacecadet-clock-report ()
  "Generate a clock report across all org files."
  (require 'org-clock)
  (let ((entries '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (let* ((heading (org-get-heading t t t t))
                    (todo-state (org-get-todo-state))
                    (clocksum (org-clock-sum-current-item)))
               (when (and todo-state (> clocksum 0))
                 (let ((hours (/ clocksum 60))
                       (mins (% clocksum 60)))
                   (push (list (cons 'heading heading)
                               (cons 'todo todo-state)
                               (cons 'minutes clocksum)
                               (cons 'formatted (format "%d:%02d" hours mins))
                               (cons 'file (buffer-file-name)))
                         entries)))))))))
    (princ (spacecadet--json-list (nreverse entries)))))

;;; ============================================================
;;; NOTE & PROPERTY OPERATIONS
;;; ============================================================

(defun spacecadet-add-note-from-env ()
  "Add a note to a task. SC_ID or SC_HEADING, plus SC_NOTE from environment."
  (let ((note (spacecadet--env "SC_NOTE"))
        (timestamp (format-time-string "%Y-%m-%d %a %H:%M")))
    (unless note
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "SC_NOTE required"))))
      (kill-emacs 1))
    (let ((found (spacecadet--find-task-smart
                  (lambda ()
                    (let ((beg (org-entry-beginning-position))
                          (end (org-entry-end-position)))
                      (goto-char beg)
                      (if (re-search-forward "^[ \t]*:LOGBOOK:" end t)
                          (progn
                            (forward-line 1)
                            (insert (format "   - Note taken on [%s] \\\\\n     %s\n"
                                            timestamp note)))
                        (goto-char beg)
                        (org-end-of-meta-data t)
                        (insert (format "   :LOGBOOK:\n   - Note taken on [%s] \\\\\n     %s\n   :END:\n"
                                        timestamp note))))
                    (save-buffer)))))
      (if found
          (princ (json-encode (list (cons 'status "ok")
                                    (cons 'note-added t))))
        (princ (json-encode (list (cons 'status "error")
                                  (cons 'message "Task not found"))))))))

(defun spacecadet-set-property-from-env ()
  "Set a property on a task. SC_ID or SC_HEADING, plus SC_PROPERTY, SC_VALUE from environment."
  (let ((property (spacecadet--env "SC_PROPERTY"))
        (value (spacecadet--env "SC_VALUE")))
    (unless (and property value)
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "SC_PROPERTY and SC_VALUE required"))))
      (kill-emacs 1))
    (let ((found (spacecadet--find-task-smart
                  (lambda ()
                    (org-set-property property value)
                    (save-buffer)))))
      (if found
          (princ (json-encode (list (cons 'status "ok")
                                    (cons 'property property)
                                    (cons 'value value))))
        (princ (json-encode (list (cons 'status "error")
                                  (cons 'message "Task not found"))))))))

;;; ============================================================
;;; REFILE OPERATION
;;; ============================================================

(defun spacecadet-refile-task-from-env ()
  "Refile a task under a target heading.
SC_ID or SC_HEADING, SC_TARGET_HEADING, SC_TARGET_FILE from environment."
  (let* ((target-heading (spacecadet--env "SC_TARGET_HEADING"))
         (target-file (spacecadet--env "SC_TARGET_FILE"))
         (target-pos nil)
         (target-buf nil)
         (source-buf nil)
         (subtree-text nil))
    (unless target-heading
      (princ (json-encode (list (cons 'status "error")
                                (cons 'message "SC_TARGET_HEADING required"))))
      (kill-emacs 1))
    ;; Find the target heading
    (let ((files (if target-file
                     (list (expand-file-name target-file spacecadet-org-dir))
                   (org-agenda-files))))
      (dolist (file files)
        (when (and (not target-pos) (file-exists-p file))
          (with-current-buffer (find-file-noselect file)
            (org-mode)
            (goto-char (point-min))
            (when (re-search-forward
                   (format "^\\*+[ \t]+%s" (regexp-quote target-heading))
                   nil t)
              (setq target-pos (point)
                    target-buf (current-buffer)))))))
    (if (not target-pos)
        (princ (json-encode (list (cons 'status "error")
                                  (cons 'message
                                        (format "Target heading not found: %s"
                                                target-heading)))))
      ;; Find and cut the source task using smart lookup
      (let ((found (spacecadet--find-task-smart
                    (lambda ()
                      (setq source-buf (current-buffer))
                      (let* ((beg (org-entry-beginning-position))
                             (end (org-entry-end-position))
                             (text (buffer-substring beg end)))
                        (setq subtree-text text)
                        (delete-region beg end)
                        (when (and (bolp) (eolp) (not (bobp)) (not (eobp)))
                          (delete-char 1)))))))
        (if (not found)
            (princ (json-encode (list (cons 'status "error")
                                      (cons 'message "Task not found"))))
          (with-current-buffer target-buf
            (goto-char target-pos)
            (let* ((target-level (org-current-level))
                   (child-level (1+ target-level)))
              (org-end-of-subtree t t)
              (let ((adjusted-text
                     (with-temp-buffer
                       (insert subtree-text)
                       (goto-char (point-min))
                       (while (re-search-forward "^\\(\\*+\\)" nil t)
                         (let* ((stars (match-string 1))
                                (new-level (+ (length stars)
                                              (- child-level 1)))
                                (new-stars (make-string new-level ?*)))
                           (replace-match new-stars)))
                       (buffer-string))))
                (unless (bolp) (insert "\n"))
                (insert adjusted-text)
                (unless (bolp) (insert "\n"))))
            (save-buffer))
          (when source-buf
            (with-current-buffer source-buf
              (save-buffer)))
          (princ (json-encode (list (cons 'status "ok")
                                    (cons 'refiled-to target-heading)))))))))

(provide 'spacecadet-init)
;;; init.el ends here

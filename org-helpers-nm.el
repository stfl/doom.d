;;; org-helpers-nm.el -*- lexical-binding: t; -*-

(defun nm/org-insert-timestamp ()
  "Insert active timestamp at POS."
  (interactive)
  (insert (format "<%s> " (format-time-string "%Y-%m-%d %H:%M:%p"))))

(defun nm/org-capture-file-picker ()
  "Select a file from the PROJECTS folder and return file-name."
  (let ((file (read-file-name "Project: " "~/.org/gtd/projects/")))
    (expand-file-name (format "%s" file))))

;; Clarify task will take a list of property fields and pass them to ~nm/org-clarify-task-properties~ to update task items which are missing those property fields.
(defun nm/org-get-headline-property (arg)
  "Extract property from headline and return results."
  (interactive)
  (org-entry-get nil arg t))

(defun nm/org-get-headline-properties ()
  "Get headline properties for ARG."
  (org-back-to-heading)
  (org-element-at-point))

(defun nm/org-get-headline-title ()
  "Get headline title from current headline."
  (interactive)
  (org-element-property :title (nm/org-get-headline-properties)))

;; Clarify Task Properties
(defun nm/update-task-tags ()
  "Update all child tasks in buffer that are missing a TAG value."
  (interactive)
  (org-show-all)
  (while (not (eobp))
    (progn
      (outline-next-heading)
      (org-narrow-to-subtree)
      (unless (eobp)
        (if (and (oh/is-task-p) (null (org-get-tags)))
            (counsel-org-tag)))
      (widen))))


(defun nm/org-clarify-task-properties (arg)
  "Update the metadata for a task headline."
  (unless (equal major-mode 'org-mode)
    (error "Not visiting an org-mode buffer."))
  (save-restriction
    (save-excursion
      (org-show-all)
      (goto-char (point-min))
      (let ((props arg))
        (while (not (eobp))
          (outline-next-heading)
          (org-narrow-to-subtree)
          (unless (eobp)
            (when (or (and (oh/is-project-p) (oh/is-todo-p)) (and (oh/is-task-p) (null (oh/has-parent-project-p)) (null (oh/has-subtask-p))))
              (mapcar (lambda (props)
                        (when (null (org-entry-get nil (upcase props) t))
                          (org-set-property (upcase props) (org-read-property-value (upcase props))))) props))
            (when (and (oh/is-todo-p) (not (oh/is-task-p)))
              (org-todo "PROJ"))
            (widen)))))))

(defun nm/org-assign-tasks-proj ()
  "Scans buffer and assigns all tasks that contain child-tasks the PROJ keyword and vice versa."
  (interactive)
  (save-excursion
    (goto-line 1)
    (while (not (eobp))
      (outline-next-heading)
      (unless (eobp)
        (when (and (oh/is-todo-p) (not (oh/is-task-p)))
          (org-todo "PROJ"))
        (when (and (equal (org-get-todo-state) "PROJ") (oh/is-task-p))
          (org-todo "TODO"))
        ;; (nm/org-set-next-state)
        ))))

(defun nm/org-set-next-state ()
  "If task contains checkbox  that's not DONE then set task state to NEXT."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (when (and (bh/is-task-p) (or (not (nm/org-checkbox-done-exist-p)) (nm/org-checkbox-exist-p)))
      (org-todo "NEXT"))
    (when (and (not (equal (org-get-todo-state) "DONE")) (bh/is-task-p) (not (nm/org-checkbox-done-exist-p)) (not (nm/org-checkbox-exist-p)))
      (org-todo "TODO"))
    (when (and (bh/is-task-p) (not (nm/org-checkbox-exist-p)) (nm/org-checkbox-done-exist-p))
      (org-todo "DONE"))))

(defun nm/org-checkbox-exist-p ()
  "Checks if a checkbox that's not marked DONE exist in the tree."
  (interactive)
  (org-back-to-heading)
  (let ((end (save-excursion (org-end-of-subtree t))))
    (search-forward-regexp "^[-+] \\[\\W].+\\|^[1-9].\\W\\[\\W]" end t)))

(defun nm/org-checkbox-done-exist-p ()
  "Checks if a checkbox that's not marked DONE exist in the tree."
  (interactive)
  (org-back-to-heading)
  (let ((end (save-excursion (org-end-of-subtree t))))
    (search-forward-regexp "^[-+] \\[X].+\\|^[1-9].\\W\\[X]" end t)))

(defun nm/org-clarify-metadata ()
  "Runs the clarify-task-metadata function with ARG being a list of property values."
  (interactive)
  (nm/org-clarify-task-properties org-tasks-properties-metadata))

(defun nm/org-capture-system ()
  "Capture stuff."
  (interactive)
  (save-restriction
    (let ((org-capture-templates
           '(("h" "headline capture" entry (function counsel-outline)
              "* %?" :empty-lines-before 1 :empty-lines-after 1)
             ("p" "plain capture" plain (function end-of-buffer)
              "<%<%Y-%m-%d %H:%M>> %?" :empty-lines-before 1 :empty-lines-after 1))))
      (find-file-other-window (read-file-name "file: " "~/.org/"))
      (if (counsel-outline-candidates)
          (org-capture nil "h"))
      (org-capture nil "p"))))

(defun nm/org-capture-to-file ()
  "Capture stuff."
  (interactive)
  (save-restriction
    (let ((org-capture-templates
           '(("h" "headline capture" entry (function counsel-outline)
              "* %?" :empty-lines-before 1 :empty-lines-after 1)
             ("p" "plain capture" plain (function end-of-buffer)
              "<%<%Y-%m-%d %H:%M>> %?" :empty-lines-before 1 :empty-lines-after 1))))
      (org-capture nil "h"))))

(defun nm/org-capture-weeklies ()
  "Find weeklies file and call counsel-outline."
  (interactive)
  (org-open-file "~/.org/gtd/weeklies.org")
  (counsel-outline))

(defun nm/org-end-of-headline()
  "Move to end of current headline"
  (interactive)
  (outline-next-heading)
  (forward-char -1))

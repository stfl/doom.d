(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" "*" "+" ":"))
 '(org-agenda-custom-commands
   '(("z" "Super zaen view"
      ((agenda ""
               ((org-agenda-span '1)
                (org-agenda-start-day
                 (org-today))))
       (tags-todo "-SOMEDAY"
                  ((org-agenda-overriding-header "")
                   ;; (org-agenda-skip-function 'bh/is-project-subtree-p)
                   (org-super-agenda-groups
                    '((:discard (:scheduled t
                                 :deadline t))
                      (:name "Next Actions"
                       :todo "NEXT"
                       :order 1)
                      (:name "Waiting for"
                       :todo "WAIT"
                       :order 20)
                      (:name "Projects"
                       :and (:todo "PROJ"
                             :children ("NEXT" "WAIT"))
                       :order 50)
                      (:name "Stuck Projects"  ;; consume the rest of PROJ -> move up with order
                       :todo "PROJ"
                       :order 40)
                      (:discard (:anything t))
                      ))
                   (org-tags-match-list-sublevels t)
                   (org-agenda-todo-ignore-with-date t)
                   (org-agenda-dim-blocked-tasks 'invisible)
                   ))))
     ("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("i" "Inbox"
      ((alltodo ""
                ((org-agenda-overriding-header "Inbox Tasks")
                 (org-agenda-files
                  '("~/.org/gtd/inbox.org" "~/.org/gtd/inbox-orgzly.org"))))
       (tags "REFILE"
             ((org-agenda-overriding-header "Refile")
              (org-tags-match-list-sublevel nil))))
      nil nil)
     ("w" "Master Agenda"
      ((agenda ""
               ((org-agenda-span '1)
                (org-agenda-start-day
                 (org-today))))
       (tags-todo "-CANCELLED/!NEXT"
                  ((org-agenda-overriding-header
                    (concat "Project Next Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                   (org-tags-match-list-sublevels t)
                   (org-agenda-todo-ignore-with-date t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down category-keep priority-up))))
       (tags-todo "-CANCELLED-SOMEDAY/!"
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-HOLD-CANCELLED-SOMEDAY/!"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-skip-function 'bh/skip-non-projects)
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-SOMEDAY-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Project Subtasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" "(including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-non-project-tasks)
                   (org-agenda-todo-ignore-with-date t)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-SOMEDAY-REFILE-CANCELLED-#waiting-#hold-#monitor/!"
                  ((org-agenda-overriding-header
                    (concat "Standalone Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-project-tasks)
                   (org-agenda-todo-ignore-with-date t)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "+WAITING|WAIT"
                  ((org-agenda-overriding-header
                    (concat "Waiting and Postponed Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function 'bh/skip-non-tasks)
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-todo-ignore-scheduled t)
                   (org-agenda-todo-ignore-deadlines t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down category-keep priority-up))))
       (tags "-REFILE/"
             ((org-agenda-overriding-header "Tasks to Archive")
              (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
              (org-tags-match-list-sublevels nil))))
      nil)))
 '(org-stuck-projects '("-SOMEDAY/+PROJ" ("NEXT" "WAIT") ("WAITING") ""))
 '(package-selected-packages '(org-roam-server)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

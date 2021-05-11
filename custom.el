(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-interactive-popup-errors nil)
 '(org-agenda-files
   '("~/.org/gtd/inbox.org" "~/.org/gtd/tickler.org" "~/.org/calendar.org" "~/.org/gtd/todo.org" "/home/stefan/.org/gtd/projects/geschenke.org" "/home/stefan/.org/gtd/projects/groceries.org" "/home/stefan/.org/gtd/projects/media.org" "/home/stefan/.org/gtd/projects/org-setup.org" "/home/stefan/.org/gtd/projects/pulswerk.org" "/home/stefan/.org/gtd/projects/sustaindock.org" "/home/stefan/.org/gtd/projects/versicherung.org"))
 '(org-level-color-stars-only nil)
 '(org-ql-views
   '(("All current TODO" :title "All not scheduled TODOs" :buffers-files org-agenda-files :query
      (and
       (todo)
       (not
        (tags "SOMEDAY"))
       (not
        (and
         (todo "TODO")
         (ancestors
          (todo "PROJ"))))
       (not
        (scheduled :from 1))
       (not
        (deadline :from 7)))
      :sort org-ql--priority< :super-groups org-super-agenda-groups :narrow nil)
     ("Current Prio B+" :title "Prio [B+]" :buffers-files org-agenda-files :query
      (and
       (todo)
       (or
        (priority >= "B")
        (ancestors
         (priority >= "B")))
       (not
        (tags "SOMEDAY"))
       (not
        (and
         (todo "TODO")
         (ancestors
          (todo "PROJ"))))
       (not
        (scheduled :from 1))
       (not
        (deadline :from 7)))
      :sort
      (priority date)
      :super-groups org-super-agenda-groups :narrow nil)
     ("High Prio [A]" :title "Prio [A]" :buffers-files org-agenda-files :query
      (and
       (todo)
       (or
        (priority >= "A")
        (ancestors
         (priority >= "A")))
       (not
        (tags "SOMEDAY"))
       (not
        (and
         (todo "TODO")
         (ancestors
          (todo "PROJ"))))
       (not
        (scheduled :from 1))
       (not
        (deadline :from 7)))
      :sort
      (priority date)
      :super-groups org-super-agenda-groups :narrow nil)
     ("All indlucing future TODO)" :title "All TODO including future" :buffers-files org-agenda-files :query
      (and
       (todo)
       (not
        (tags "SOMEDAY"))
       (not
        (and
         (todo "TODO")
         (ancestors
          (todo "PROJ")))))
      :sort priority :super-groups org-super-agenda-groups :narrow nil)
     ("All TODO including SOMEDAY" :title "All all all" :buffers-files org-agenda-files :query
      (and
       (todo))
      :sort priority :super-groups org-super-agenda-groups :narrow nil)
     ("@home | wohnung" :title "@home | wohnung" :buffers-files org-agenda-files :query
      (and
       (todo)
       (tags "@home" "wohnung")
       (not
        (tags "SOMEDAY"))
       (not
        (and
         (todo "TODO")
         (ancestors
          (todo "PROJ"))))
       (not
        (scheduled :from today))
       (not
        (deadline :from 7)))
      :sort priority :super-groups org-super-agenda-groups :narrow nil)
     ("@office | pulswerk" :title "@office | pulswerk" :buffers-files org-agenda-files :query
      (and
       (todo)
       (tags "@office" "pulswerk")
       (not
        (tags "SOMEDAY"))
       (not
        (and
         (todo "TODO")
         (ancestors
          (todo "PROJ"))))
       (not
        (scheduled :from today))
       (not
        (deadline :from 7)))
      :sort priority :super-groups org-super-agenda-groups :narrow nil)
     ("Projects" :buffers-files org-agenda-files :query
      (and
       (todo "PROJ")
       (not
        (tags "SOMEDAY")))
      :title "Projects" :sort priority :super-groups org-super-agenda-groups :narrow nil)
     ("Overview: Agenda-like" :buffers-files org-agenda-files :query
      (and
       (not
        (done))
       (or
        (habit)
        (scheduled :to 0)
        (ts-active :on today)))
      :title "Agenda-like" :sort
      (date priority todo)
      :super-groups org-super-agenda-groups)
     ("Overview: NEXT tasks" :buffers-files org-agenda-files :query
      (todo "NEXT")
      :title "Overview: NEXT tasks" :sort
      (priority date)
      :super-groups org-super-agenda-groups)
     ("Calendar: Today" :buffers-files org-agenda-files :query
      (ts-active :on today)
      :title "Today" :sort
      (priority)
      :super-groups org-super-agenda-groups)
     ("Review: Recently timestamped" . org-ql-view-recent-items)
     ("Review: Dangling tasks" :buffers-files org-agenda-files :query
      (and
       (todo)
       (ancestors
        (done)))
      :title "Review: Dangling tasks" :sort
      (date priority todo)
      :super-groups
      ((:auto-parent t)))
     ("Review: Stale tasks" :buffers-files org-agenda-files :query
      (and
       (todo)
       (not
        (ts :from -14)))
      :title "Review: Stale tasks" :sort
      (date priority todo)
      :super-groups
      ((:auto-parent t)))
     ("Review: Stuck projects" :buffers-files org-agenda-files :query
      (and
       (todo)
       (descendants
        (todo))
       (not
        (descendants
         (todo "NEXT"))))
      :title "Review: Stuck projects" :sort
      (priority date)
      :super-groups org-super-agenda-groups)))
 '(org-stuck-projects '("-SOMEDAY/+PROJ" ("NEXT" "WAIT") ("WAITING") ""))
 '(package-selected-packages '(org-roam-server))
 '(warning-suppress-types '(((org-roam)) ((org-roam)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-drawer ((t (:foreground "dark gray" :height 0.8))))
 '(org-level-1 ((t (:inherit outline-1 :extend t :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :extend t :height 1.25))))
 '(org-level-3 ((t (:inherit outline-3 :extend t :height 1.15))))
 '(org-level-4 ((t (:inherit outline-4 :extend t :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :extend t))))
 '(org-level-6 ((t (:inherit outline-6 :extend t))))
 '(org-level-7 ((t (:inherit outline-7 :extend t))))
 '(org-level-8 ((t (:inherit outline-8 :extend t))))
 '(org-property-value ((t (:height 0.85))) t)
 '(org-special-keyword ((t (:foreground "#83898d" :height 0.8)))))

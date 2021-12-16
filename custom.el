(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-interactive-popup-errors nil)
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
     ("Review: Stuck Projects" :buffers-files org-agenda-files :query
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
 '(safe-local-variable-values
   '((max-specpd-size . 100000)
     (max-specpd-size . 20000)
     (authinfo-mode . t)
     (org-use-tag-inheritance)
     (lsp-file-watch-ignored-directories . t)
     (lsp-file-watch-threshold . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-date ((t (:foreground "dark goldenrod" :height 0.85))))
 '(org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.8))))
 '(org-drawer ((t (:foreground "dark gray" :height 0.8))))
 '(org-property-value ((t (:height 0.85))) t)
 '(org-ql-view-due-date ((t (:foreground "dark goldenrod"))))
 '(org-special-keyword ((t (:foreground "#83898d" :height 0.8))))
 '(org-tag ((t (:foreground "#83898d" :weight light :height 0.7))))
 '(outline-1 ((t (:height 1.5))))
 '(outline-2 ((t (:height 1.25))))
 '(outline-3 ((t (:height 1.15)))))

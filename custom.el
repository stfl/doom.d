(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   '(("ho" "At the office" tags-todo "@office"
      ((org-agenda-overriding-header "Office")
       (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
     ("n" "Agenda and all TODOs"
      ((agenda ""
               ((org-agenda-overriding-header "Today")
                (org-agenda-span 'day)
                (org-deadline-warning-days 1)
                (org-agenda-overriding-header "")))
       (alltodo "" nil))
      nil)))
 '(package-selected-packages '(org-roam-server)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

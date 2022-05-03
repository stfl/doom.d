(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-interactive-popup-errors nil)
 '(safe-local-variable-values
   '((auto-revert-mode . 1)
     (eval if
           (boundp 'lsp-file-watch-ignored-directories)
           (set
            (make-local-variable 'lsp-file-watch-ignored-directories)
            (push "[/\\\\]uploads\\" lsp-file-watch-ignored-directories))
           (after! lsp-mode
             (set
              (make-local-variable 'lsp-file-watch-ignored-directories)
              (push "[/\\\\]uploads\\" lsp-file-watch-ignored-directories))))
     (eval set
           (make-local-variable 'lsp-file-watch-ignored-directories)
           (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]uploads\\"))
     (max-specpd-size . 100000)
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
 '(+org-priority-a ((t (:foreground "red3" :weight bold :height 0.95))))
 '(+org-priority-b ((t (:foreground "OrangeRed2" :weight bold))))
 '(+org-priority-c ((t (:foreground "DarkOrange2" :weight bold))))
 '(+org-priority-d ((t (:foreground "gold3" :weight bold))))
 '(+org-priority-e ((t (:foreground "OliveDrab1" :weight bold))))
 '(+org-priority-f ((t (:foreground "SpringGreen3" :weight bold))))
 '(+org-priority-g ((t (:foreground "cyan4" :weight bold))))
 '(+org-priority-h ((t (:foreground "DeepSkyBlue4" :weight bold))))
 '(+org-priority-i ((t (:foreground "LightSteelBlue3" :weight bold))))
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

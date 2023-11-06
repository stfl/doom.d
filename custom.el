(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "ac18cc10455c6c26a98354ba8f9d338842d7ecc9ae3d28c205ed154ef20d74ce" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((org-time-stamp-rounding-minutes 0 15)
     (org-clock-rounding-minutes . 15)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
      (add-hook 'write-contents-functions
                (lambda nil
                  (delete-trailing-whitespace)
                  nil))
      (require 'whitespace)
      "Sometimes the mode needs to be toggled off and on."
      (whitespace-mode 0)
      (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail))))
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
 '(adoc-bold-face ((t (:bold t))))
 '(adoc-code-face ((t (:inherit org-block))))
 '(adoc-complex-replacement-face ((t (:inherit org-code :bold t))))
 '(adoc-emphasis-face ((t (:inherit org-verbatim))))
 '(adoc-internal-reference-face ((t (:inherit org-link))))
 '(adoc-meta-face ((t (:inherit org-meta-line))))
 '(adoc-reference-face ((t (:inherit org-link))))
 '(adoc-typewriter-face ((t (:inherit org-code))))
 '(adoc-verbatim-face ((t (:inherit org-verbatim))))
 '(adoc-warning-face ((t (:inherit org-warning))))
 '(notmuch-message-summary-face ((t (:foreground "#848d94"))))
 '(notmuch-search-unread-face ((t (:foreground "#bbc2cf" :weight 'bold))))
 '(notmuch-tree-no-match-face ((t (:foreground "#848d94"))))
 '(notmuch-wash-cited-text ((t (:foreground "#73797e"))))
 '(org-code ((t (:foreground "#f1d1a2" :extend t))))
 '(org-date ((t (:foreground "dark goldenrod" :height 0.85))))
 '(org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.8))))
 '(org-drawer ((t (:foreground "dark gray" :height 0.8))))
 '(org-property-value ((t (:height 0.85))))
 '(org-ql-view-due-date ((t (:foreground "dark goldenrod"))))
 '(org-special-keyword ((t (:foreground "#83898d" :height 0.8))))
 '(org-tag ((t (:foreground "#83898d" :weight light :height 0.7))))
 '(outline-1 ((t (:height 1.5))))
 '(outline-2 ((t (:height 1.25))))
 '(outline-3 ((t (:height 1.15))))
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
